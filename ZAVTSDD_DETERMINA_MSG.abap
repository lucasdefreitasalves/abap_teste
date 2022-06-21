*&---------------------------------------------------------------------*
*& Include ZAVTMMD_DETERMINA_MSG
*&---------------------------------------------------------------------*

teste lucas

*IF sy-uname = 'NCOELHO'.
*** Declaração de Types Local
TYPES: BEGIN OF ty_texto,
         nivel    TYPE zavtsdt_det_msg-nivel,
         msg      TYPE string,
*20191018IHT - Mensagens Nota fiscal - BEGIN.
         itmnum   TYPE j_1bitmnum,
         header   TYPE flag,
         ztipomsg TYPE ztipomsg,
*20191018IHT - Mensagens Nota fiscal - END.
       END OF ty_texto.

*** Declaração de tabela Local
DATA: lt_msg TYPE TABLE OF ty_texto.

*** Declaração de estrutura local
DATA: lwa_msg TYPE ty_texto.
*20191018IHT - Mensagens Nota fiscal - BEGIN.
DATA: msg_parametros TYPE string,
      msg_texto      TYPE string,
      v_taxval       TYPE j_1btaxval.
*20191018IHT - Mensagens Nota fiscal - END.

CONSTANTS: c_direct_2 TYPE char1 VALUE '2',
           c_fret     TYPE string VALUE 'FRET',
           c_a        VALUE 'A'.

DATA: lwa_shipment_data        TYPE vt04_shipment,
      v_frete_liq              TYPE rv54_frinf_def-result,
      v_base_calc              TYPE rv54_frinf_def-result,
      v_aliquota_imposto       TYPE rv54_frinf_def-result,
      v_frete_liq_c(18)        TYPE c,
      v_base_calc_c(18)        TYPE c,
      v_aliquota_imposto_c(18) TYPE c,
      v_imposto_c(18)          TYPE c,

      v_imposto                TYPE rv54_frinf_def-result,
      lv_tknum                 TYPE tknum,
      lv_texto_adic(255)       TYPE c.

DATA: o_badi_shpmnt_extension TYPE REF TO if_ex_le_shpmnt_extension.

DATA:
      ls_fhg_ajus_e111 TYPE ztfhg_ajus_e111.

IF it_vbrp IS NOT INITIAL.
*** Sales Document: Header Data
  SELECT vbeln,
         auart,
         augru,
         bstnk
       FROM vbak
       INTO TABLE @DATA(lt_vbak)
       FOR ALL ENTRIES IN @it_vbrp
       WHERE vbeln EQ @it_vbrp-aubel.

ENDIF.
*** Plants/Branches
SELECT werks,
       regio
  FROM t001w
  INTO TABLE @DATA(lt_t001w)
  FOR ALL ENTRIES IN @it_nflin
  WHERE werks EQ @it_nflin-werks.

IF <ts_wnfnad> IS ASSIGNED.

  CLEAR: lwa_regio.
  lwa_regio-option = 'EQ'.
  lwa_regio-sign   = 'I'.

  LOOP AT <ts_wnfnad> ASSIGNING FIELD-SYMBOL(<lfs_wnfnad>).

    IF <lfs_wnfnad>-parvw EQ 'SP'.

      lwa_regio-low = <lfs_wnfnad>-regio.
      APPEND lwa_regio TO lr_regio.

    ENDIF.

  ENDLOOP.

ENDIF.

*IF it_vbrp   IS NOT INITIAL OR
*   lr_aubel  IS NOT INITIAL OR
*   lr_werks  IS NOT INITIAL OR
*   is_header IS NOT INITIAL OR
*   lr_regio  IS NOT INITIAL.
*IF is_header-direct = c_direct_2.

*** Determinação de mensagens
SELECT *
  FROM zavtsdt_det_msg
  INTO TABLE @DATA(lt_det_msg).

IF sy-subrc EQ 0.

  LOOP AT lt_det_msg INTO DATA(lwa_det_msg).

    DATA(l_tabix) = sy-tabix.

*** a.  UF Origem
    READ TABLE lt_t001w WITH KEY regio = lwa_det_msg-uf_origem
                        TRANSPORTING NO FIELDS.

    IF sy-subrc NE 0 AND lwa_det_msg-uf_origem IS NOT INITIAL.

      DELETE lt_det_msg INDEX l_tabix.
      CLEAR: l_tabix.
      CONTINUE.

    ENDIF.

*** b.  UF Destino
    IF lwa_det_msg-uf_destino NE is_header-regio AND
       lwa_det_msg-uf_destino IS NOT INITIAL.

      DELETE lt_det_msg INDEX l_tabix.
      CLEAR: l_tabix.
      CONTINUE.

    ENDIF.

*** c.  UF Transportadora
    READ TABLE lr_regio WITH KEY low =  lwa_det_msg-uf_transp
                        TRANSPORTING NO FIELDS.

    IF sy-subrc NE 0 AND lwa_det_msg-uf_transp IS NOT INITIAL.

      DELETE lt_det_msg INDEX l_tabix.
      CLEAR: l_tabix.
      CONTINUE.

    ENDIF.

*** d.  Tipo Documento de Vendas
    READ TABLE lt_vbak WITH KEY auart = lwa_det_msg-auart
                       TRANSPORTING NO FIELDS.

    IF sy-subrc NE 0 AND lwa_det_msg-auart IS NOT INITIAL.

      DELETE lt_det_msg INDEX l_tabix.
      CLEAR: l_tabix.
      CONTINUE.

    ENDIF.

*** e.  Motivo da Ordem
    READ TABLE lt_vbak WITH KEY augru = lwa_det_msg-augru
                       TRANSPORTING NO FIELDS.

    IF sy-subrc NE 0 AND lwa_det_msg-augru IS NOT INITIAL.

      DELETE lt_det_msg INDEX l_tabix.
      CLEAR: l_tabix.
      CONTINUE.

    ENDIF.

*** f.  Categoria de Item
    READ TABLE it_vbrp WITH KEY pstyv = lwa_det_msg-pstyv
                       TRANSPORTING NO FIELDS.

    IF sy-subrc NE 0 AND lwa_det_msg-pstyv IS NOT INITIAL.

      DELETE lt_det_msg INDEX l_tabix.
      CLEAR: l_tabix.
      CONTINUE.

    ENDIF.

*** g.  Centro
    READ TABLE it_nflin WITH KEY werks = lwa_det_msg-werks
                       TRANSPORTING NO FIELDS.

    IF sy-subrc NE 0 AND lwa_det_msg-werks IS NOT INITIAL.

      DELETE lt_det_msg INDEX l_tabix.
      CLEAR: l_tabix.
      CONTINUE.

    ENDIF.

*** h.  Origem Material
    READ TABLE it_nflin WITH KEY matorg = lwa_det_msg-mtorg
                       TRANSPORTING NO FIELDS.

    IF sy-subrc NE 0 AND lwa_det_msg-mtorg IS NOT INITIAL.

      DELETE lt_det_msg INDEX l_tabix.
      CLEAR: l_tabix.
      CONTINUE.

    ENDIF.

*** i.  Grupo de Mercadoria
    READ TABLE it_nflin WITH KEY matkl = lwa_det_msg-matkl
                       TRANSPORTING NO FIELDS.

    IF sy-subrc NE 0 AND lwa_det_msg-matkl IS NOT INITIAL.

      DELETE lt_det_msg INDEX l_tabix.
      CLEAR: l_tabix.
      CONTINUE.

    ENDIF.

  ENDLOOP.

*** Deixa as mensagens em ordem de maior para menor
  SORT: lt_det_msg BY ztipomsg nivel ASCENDING.
*** Deixa apenas o maior nivel de cada tipo de mensagem
  DELETE ADJACENT DUPLICATES FROM lt_det_msg COMPARING ztipomsg.

  LOOP AT lt_det_msg INTO lwa_det_msg.
    CLEAR: l_tdname.
    l_tdname = lwa_det_msg-id_msg.

*  ** SAPscript: Read text
    FREE:  lt_tline.
    CLEAR: lwa_thead,
           lwa_thead_tdt.
    CALL FUNCTION 'READ_TEXT'
      EXPORTING
        client                  = sy-mandt
        id                      = 'ST'
        language                = 'P'
        name                    = l_tdname
        object                  = 'TEXT'
      IMPORTING
        header                  = lwa_thead
        old_line_counter        = lwa_thead_tdt
      TABLES
        lines                   = lt_tline
      EXCEPTIONS
        id                      = 1
        language                = 2
        name                    = 3
        not_found               = 4
        object                  = 5
        reference_check         = 6
        wrong_access_to_archive = 7
        OTHERS                  = 8.

    IF sy-subrc EQ 0.

      LOOP AT lt_tline INTO DATA(lwa_tline).

        lwa_msg-ztipomsg = lwa_det_msg-ztipomsg.

*  20191018IHT - Mensagens Nota fiscal - BEGIN.
        IF lwa_det_msg-kschl IS NOT INITIAL.
          IF lwa_det_msg-header IS INITIAL.
            LOOP AT et_item INTO DATA(lwa_item).
              READ TABLE it_nfstx INTO DATA(lwa_nfstx) WITH KEY itmnum = lwa_item-itmnum taxtyp = lwa_det_msg-kschl.
              IF sy-subrc IS INITIAL .
                msg_texto = lwa_tline-tdline.
                msg_parametros = lwa_nfstx-base.
                REPLACE '&BASE&' IN msg_texto WITH msg_parametros.
                REPLACE '.' IN msg_texto WITH ','.
                msg_parametros = lwa_nfstx-rate.
                REPLACE '&RATE&' IN msg_texto WITH msg_parametros.
                REPLACE '.' IN msg_texto WITH ','.
                msg_parametros = lwa_nfstx-taxval.
                REPLACE '&TAXVAL&' IN msg_texto WITH msg_parametros.
                REPLACE '.' IN msg_texto WITH ','.
                CONDENSE msg_texto.
                lwa_msg-nivel = lwa_det_msg-nivel.
                lwa_msg-msg   = msg_texto.
                lwa_msg-itmnum = lwa_item-itmnum.
                lwa_msg-header = lwa_det_msg-header.
                APPEND lwa_msg TO lt_msg.
              ENDIF.
            ENDLOOP.
          ELSE.
            IF lwa_det_msg-kschl = c_fret.
              READ TABLE lt_t001w INTO DATA(lwa_t001) INDEX 1.
              READ TABLE lr_regio INTO lwa_regio INDEX 1.
              IF  lwa_t001-regio IS NOT INITIAL AND
                  lwa_regio-low   IS NOT INITIAL.
                SELECT shipfrom,
                       shipto,
                       validfrom,
                       rate_f
                UP TO 1 ROWS
                FROM j_1btxic1
                INTO @DATA(wa_j_1btxic1)
                WHERE shipfrom = @lwa_t001-regio
                    AND shipto = @is_header-regio.
                ENDSELECT.

                IF lv_tknum1 IS NOT INITIAL.

                  SELECT *
                    FROM vttk
                    INTO TABLE lwa_shipment_data-xvttk
                    WHERE tknum = lv_tknum1.

                  CALL FUNCTION 'RV_SHIPMENT_VIEW'
                    EXPORTING
                      shipment_number          = lv_tknum1
                      option_tvtk              = abap_true
                      option_ttds              = abap_true
                      language                 = sy-langu
                      option_items             = abap_true
                      option_stawn_read        = abap_true
                      option_segments          = abap_true
                      option_partners          = abap_true
                      option_messages          = abap_true
                      option_package_dialog    = abap_true
                      option_flow              = abap_true
                      option_authority_check   = abap_true
                      activity                 = c_a "'A'
                      gi_badi_shpmnt_extension = o_badi_shpmnt_extension
                    IMPORTING
                      f_tvtk                   = lwa_shipment_data-tvtk_wa
                      f_ttds                   = lwa_shipment_data-ttds_wa
                    TABLES
                      f_vttp                   = lwa_shipment_data-xvttp
                      f_trlk                   = lwa_shipment_data-xtrlk
                      f_trlp                   = lwa_shipment_data-xtrlp
                      f_vtts                   = lwa_shipment_data-xvtts
                      f_vtsp                   = lwa_shipment_data-xvtsp
                      f_vbpa                   = lwa_shipment_data-xvbpa
                    EXCEPTIONS
                      not_found                = 1
                      no_authority             = 2
                      delivery_missing         = 3
                      delivery_lock            = 4
                      OTHERS                   = 99.

                  CALL FUNCTION 'SD_SCD_COST_INFO_SHIPMENT'
                    EXPORTING
                      i_shipment_data  = lwa_shipment_data
                      i_opt_batch_mode = abap_true
                      i_opt_sim        = 3
                    IMPORTING
                      e_result         = v_frete_liq.

                  IF v_frete_liq IS NOT INITIAL.
                    v_aliquota_imposto = wa_j_1btxic1-rate_f / 100.
                    IF v_aliquota_imposto <> 1.
                      v_base_calc = v_frete_liq / ( 1 - v_aliquota_imposto ).
                      v_imposto = v_base_calc - v_frete_liq.
                    ENDIF.
                  ENDIF.

                  v_frete_liq_c = v_frete_liq.
                  v_base_calc_c = v_base_calc.
                  v_aliquota_imposto_c = v_aliquota_imposto * 100.
                  v_imposto_c = v_imposto.

                  CONDENSE: v_frete_liq_c NO-GAPS,
                            v_base_calc_c NO-GAPS,
                            v_aliquota_imposto_c NO-GAPS,
                            v_imposto_c NO-GAPS.

                  msg_texto = lwa_tline-tdline.
                  msg_parametros = v_base_calc_c.
                  REPLACE '&BASE&' IN msg_texto WITH msg_parametros.
                  REPLACE '.' IN msg_texto WITH ','.
                  msg_parametros = v_aliquota_imposto_c.
                  REPLACE '&RATE&' IN msg_texto WITH msg_parametros.
                  REPLACE '.' IN msg_texto WITH ','.
                  msg_parametros = v_imposto_c.
                  REPLACE '&TAXVAL&' IN msg_texto WITH msg_parametros.
                  REPLACE '.' IN msg_texto WITH ','.
                  msg_parametros = v_frete_liq_c.
                  REPLACE '&VALLIQ&' IN msg_texto WITH msg_parametros.
                  REPLACE '.' IN msg_texto WITH ','.
                  CONDENSE msg_texto.
                  lwa_msg-nivel = lwa_det_msg-nivel.
                  lwa_msg-msg   = msg_texto.
                  lwa_msg-header = lwa_det_msg-header.
                  APPEND lwa_msg TO lt_msg.

                  ls_fhg_ajus_e111-bukrs = is_header-bukrs.
                  ls_fhg_ajus_e111-branch = is_header-branch.
                  ls_fhg_ajus_e111-pstdat = is_header-pstdat.
                  ls_fhg_ajus_e111-vlr_icms = v_imposto.

                  IF ls_fhg_ajus_e111 IS NOT INITIAL.
                    EXPORT fhg_ajus_e111 FROM ls_fhg_ajus_e111 TO MEMORY ID 'ZFHG_AJUST'.
                  ENDIF.

                ENDIF.
              ENDIF.
            ELSE.
              CLEAR v_taxval.
              LOOP AT it_nfstx INTO lwa_nfstx WHERE taxtyp = lwa_det_msg-kschl.
                v_taxval = v_taxval + lwa_nfstx-taxval.
              ENDLOOP.
              IF v_taxval IS NOT INITIAL.
                msg_texto = lwa_tline-tdline.
                msg_parametros = v_taxval.
                REPLACE '&TAXVAL&' IN msg_texto WITH msg_parametros.
                REPLACE '.' IN msg_texto WITH ','.
                CONDENSE msg_texto.
                lwa_msg-nivel = lwa_det_msg-nivel.
                lwa_msg-msg   = msg_texto.
                lwa_msg-itmnum = lwa_item-itmnum.
                lwa_msg-header = lwa_det_msg-header.
                APPEND lwa_msg TO lt_msg.
              ENDIF.
            ENDIF.
          ENDIF.
        ELSE.
*  20191018IHT - Mensagens Nota fiscal - END.
          lwa_msg-nivel = lwa_det_msg-nivel.
          lwa_msg-msg   = lwa_tline-tdline.
          lwa_msg-header = lwa_det_msg-header.
          APPEND lwa_msg TO lt_msg.
        ENDIF.

      ENDLOOP.

    ENDIF.
    CLEAR: lwa_det_msg.
  ENDLOOP.

  " <- Grava Mensagem ATV_MG na tabela ZTFHG_AJUS_E111
  DATA lv_texto TYPE string.
  DATA(lt_msg_aux) = lt_msg[].
  DELETE lt_msg_aux WHERE ztipomsg NE 'ATV_MG'.

  LOOP AT lt_msg_aux INTO DATA(ls_msg_aux).
    IF ls_msg_aux-msg IS NOT INITIAL.
      CONCATENATE ls_msg_aux-msg lv_texto INTO lv_texto SEPARATED BY space.
    ENDIF.
  ENDLOOP.

  EXPORT lv_texto FROM lv_texto TO MEMORY ID 'ZFHG_TEXTO'.
  " <- Grava Mensagem ATV_MG na tabela ZTFHG_AJUS_E111

  CLEAR: lwa_msg.
  IF line_exists( it_nflin[ 1 ] ).
    IF  it_nflin[ 1 ]-refkey IS NOT INITIAL
    AND ztax_cl_1bnf_add_data=>mv_refkey <> it_nflin[ 1 ]-refkey.

      LOOP AT lt_msg INTO lwa_msg.
        ztax_cl_1bnf_add_data=>mv_refkey = it_nflin[ 1 ]-refkey.
        IF lwa_msg-header IS NOT INITIAL.

          CONCATENATE: es_header-infcpl lwa_msg-msg INTO es_header-infcpl
                                                  SEPARATED BY space.

        ELSE.
          READ TABLE et_item INTO lwa_item WITH KEY itmnum = lwa_msg-itmnum.

          IF sy-subrc IS INITIAL.

            CONCATENATE: lwa_item-infadprod lwa_msg-msg INTO lwa_item-infadprod
                                                  SEPARATED BY space.

            MODIFY et_item FROM lwa_item INDEX sy-tabix TRANSPORTING infadprod.

          ENDIF.

        ENDIF.

      ENDLOOP.
    ENDIF.
  ENDIF.
ENDIF.

LOOP AT it_nfftx INTO DATA(lwa_nfftx).

  READ TABLE it_nfref WITH KEY seqnum = lwa_nfftx-seqnum TRANSPORTING NO FIELDS.

  IF sy-subrc IS NOT INITIAL.
    CONCATENATE: es_header-infcpl lwa_nfftx-message INTO es_header-infcpl
                                            SEPARATED BY space.

  ENDIF.

ENDLOOP.

"Textos de Ref. de documentos (CR_2453)
TRY .

    DATA l_docref TYPE j_1brefkey.

    DATA(l_bstnk) = lt_vbak[ 1 ]-bstnk.
    IF l_bstnk IS NOT INITIAL.
      l_docref = |{ l_bstnk ALPHA = OUT }| .
      l_docref = l_docref && '.'.

      "Verifica se a referência não existe
      IF es_header-infcpl NS l_docref.
        CONCATENATE es_header-infcpl
                    'Pedido de Ref. do Cliente Nº'(003) l_docref
          INTO es_header-infcpl
          SEPARATED BY space.
      ENDIF.

    ENDIF.

    DATA(l_aubel) = it_vbrp[ 1 ]-aubel.
    IF l_aubel IS NOT INITIAL.
      l_docref = |{ l_aubel ALPHA = OUT }| .
      l_docref = l_docref && '.'.

      "Verifica se a referência não existe
      IF es_header-infcpl NS l_docref.
        CONCATENATE es_header-infcpl
                    'Ordem de Venda Nº'(004) l_docref
          INTO es_header-infcpl
          SEPARATED BY space.
      ENDIF.

    ENDIF.

    DATA(l_vgbel) = it_vbrp[ 1 ]-vgbel.
    IF l_vgbel IS NOT INITIAL.
      l_docref = |{ l_vgbel ALPHA = OUT }| .
      l_docref = l_docref && '.'.

      "Verifica se a referência não existe
      IF es_header-infcpl NS l_docref.
        CONCATENATE es_header-infcpl
                    'Remessa Nº'(006) l_docref
          INTO es_header-infcpl
          SEPARATED BY space.
      ENDIF.

    ENDIF.

    DATA(l_refkey) = it_nflin[ 1 ]-refkey.
    IF l_refkey IS NOT INITIAL.
      l_docref = |{ l_refkey ALPHA = OUT }| .
      l_docref = l_docref && '.'.

      "Verifica se a referência não existe
      IF es_header-infcpl NS l_docref.
        CONCATENATE es_header-infcpl
                    'Faturamento Nº'(005) l_docref
          INTO es_header-infcpl
          SEPARATED BY space.
      ENDIF.

    ENDIF.

  CATCH cx_sy_itab_line_not_found.

ENDTRY.

*ENDIF.