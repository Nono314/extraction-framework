package org.dbpedia.extraction.wikiparser.impl.wikipedia

import org.dbpedia.extraction.util.Language

/**
 * Holds the redirect identifiers of the different Wikipedia languages. Do not edit Redirect.scala!
 * GenerateWikiSettings.scala uses the template file Redirect.scala.txt to generate Redirect.scala.
 */
object Redirect
{
    private val map : Map[String, Set[String]] =
    Map("bxr"->bxr_redirects,"am"->am_redirects,"lg"->lg_redirects,"mappings"->mappings_redirects,"ig"->ig_redirects,"fa"->fa_redirects,"ay"->ay_redirects,"ru"->ru_redirects,"commons"->commons_redirects,"ca"->ca_redirects,"li"->li_redirects,"az"->az_redirects,"ii"->ii_redirects,"ff"->ff_redirects,"an"->an_redirects,"wikidata"->wikidata_redirects,"rue"->rue_redirects,"lij"->lij_redirects,"cbk-zam"->cbk_zam_redirects,"ba"->ba_redirects,"ik"->ik_redirects,"aa"->aa_redirects,"ang"->ang_redirects,"fi"->fi_redirects,"rw"->rw_redirects,"lmo"->lmo_redirects,"cdo"->cdo_redirects,"bar"->bar_redirects,"ilo"->ilo_redirects,"ab"->ab_redirects,"fiu-vro"->fiu_vro_redirects,"ar"->ar_redirects,"ln"->ln_redirects,"sa"->sa_redirects,"ce"->ce_redirects,"bat-smg"->bat_smg_redirects,"io"->io_redirects,"ace"->ace_redirects,"arc"->arc_redirects,"fj"->fj_redirects,"lo"->lo_redirects,"sah"->sah_redirects,"ceb"->ceb_redirects,"af"->af_redirects,"is"->is_redirects,"bcl"->bcl_redirects,"arz"->arz_redirects,"fo"->fo_redirects,"lt"->lt_redirects,"sc"->sc_redirects,"ch"->ch_redirects,"ak"->ak_redirects,"as"->as_redirects,"be"->be_redirects,"it"->it_redirects,"fr"->fr_redirects,"ltg"->ltg_redirects,"scn"->scn_redirects,"cho"->cho_redirects,"ast"->ast_redirects,"iu"->iu_redirects,"be-x-old"->be_x_old_redirects,"als"->als_redirects,"lv"->lv_redirects,"frp"->frp_redirects,"chr"->chr_redirects,"av"->av_redirects,"sco"->sco_redirects,"bg"->bg_redirects,"ja"->ja_redirects,"mai"->mai_redirects,"frr"->frr_redirects,"bh"->bh_redirects,"chy"->chy_redirects,"nds-nl"->nds_nl_redirects,"sd"->sd_redirects,"bo"->bo_redirects,"jbo"->jbo_redirects,"ckb"->ckb_redirects,"map-bms"->map_bms_redirects,"fur"->fur_redirects,"bi"->bi_redirects,"ne"->ne_redirects,"bpy"->bpy_redirects,"se"->se_redirects,"mdf"->mdf_redirects,"jv"->jv_redirects,"fy"->fy_redirects,"co"->co_redirects,"bjn"->bjn_redirects,"new"->new_redirects,"br"->br_redirects,"sg"->sg_redirects,"mg"->mg_redirects,"ka"->ka_redirects,"ga"->ga_redirects,"cr"->cr_redirects,"bm"->bm_redirects,"ng"->ng_redirects,"bs"->bs_redirects,"sh"->sh_redirects,"mh"->mh_redirects,"kaa"->kaa_redirects,"crh"->crh_redirects,"bn"->bn_redirects,"gag"->gag_redirects,"nl"->nl_redirects,"bug"->bug_redirects,"si"->si_redirects,"mhr"->mhr_redirects,"kab"->kab_redirects,"cs"->cs_redirects,"gan"->gan_redirects,"cy"->cy_redirects,"mk"->mk_redirects,"nn"->nn_redirects,"simple"->simple_redirects,"mi"->mi_redirects,"kbd"->kbd_redirects,"gd"->gd_redirects,"ml"->ml_redirects,"csb"->csb_redirects,"no"->no_redirects,"da"->da_redirects,"min"->min_redirects,"kg"->kg_redirects,"sk"->sk_redirects,"mn"->mn_redirects,"gl"->gl_redirects,"nov"->nov_redirects,"cu"->cu_redirects,"ki"->ki_redirects,"de"->de_redirects,"mwl"->mwl_redirects,"sl"->sl_redirects,"mo"->mo_redirects,"glk"->glk_redirects,"nrm"->nrm_redirects,"cv"->cv_redirects,"diq"->diq_redirects,"kj"->kj_redirects,"my"->my_redirects,"mr"->mr_redirects,"gn"->gn_redirects,"nso"->nso_redirects,"ee"->ee_redirects,"sm"->sm_redirects,"dsb"->dsb_redirects,"kk"->kk_redirects,"mrj"->mrj_redirects,"myv"->myv_redirects,"nv"->nv_redirects,"got"->got_redirects,"sn"->sn_redirects,"el"->el_redirects,"dv"->dv_redirects,"kl"->kl_redirects,"mzn"->mzn_redirects,"ny"->ny_redirects,"ms"->ms_redirects,"gu"->gu_redirects,"so"->so_redirects,"eml"->eml_redirects,"dz"->dz_redirects,"km"->km_redirects,"na"->na_redirects,"gv"->gv_redirects,"mt"->mt_redirects,"oc"->oc_redirects,"sq"->sq_redirects,"en"->en_redirects,"es"->es_redirects,"kn"->kn_redirects,"mus"->mus_redirects,"nah"->nah_redirects,"ha"->ha_redirects,"sr"->sr_redirects,"om"->om_redirects,"eo"->eo_redirects,"ko"->ko_redirects,"et"->et_redirects,"hak"->hak_redirects,"nap"->nap_redirects,"srn"->srn_redirects,"or"->or_redirects,"koi"->koi_redirects,"nds"->nds_redirects,"haw"->haw_redirects,"eu"->eu_redirects,"os"->os_redirects,"tn"->tn_redirects,"ss"->ss_redirects,"kr"->kr_redirects,"pap"->pap_redirects,"ext"->ext_redirects,"he"->he_redirects,"to"->to_redirects,"st"->st_redirects,"pa"->pa_redirects,"pcd"->pcd_redirects,"hi"->hi_redirects,"krc"->krc_redirects,"vls"->vls_redirects,"tpi"->tpi_redirects,"stq"->stq_redirects,"pag"->pag_redirects,"pdc"->pdc_redirects,"hif"->hif_redirects,"vo"->vo_redirects,"pam"->pam_redirects,"tr"->tr_redirects,"su"->su_redirects,"ks"->ks_redirects,"pfl"->pfl_redirects,"ho"->ho_redirects,"wa"->wa_redirects,"ts"->ts_redirects,"pnb"->pnb_redirects,"ksh"->ksh_redirects,"sv"->sv_redirects,"hr"->hr_redirects,"war"->war_redirects,"pi"->pi_redirects,"sw"->sw_redirects,"tt"->tt_redirects,"ps"->ps_redirects,"ku"->ku_redirects,"hsb"->hsb_redirects,"wo"->wo_redirects,"pih"->pih_redirects,"szl"->szl_redirects,"tum"->tum_redirects,"pt"->pt_redirects,"kv"->kv_redirects,"ht"->ht_redirects,"wuu"->wuu_redirects,"pl"->pl_redirects,"ta"->ta_redirects,"tw"->tw_redirects,"qu"->qu_redirects,"kw"->kw_redirects,"xal"->xal_redirects,"hu"->hu_redirects,"pms"->pms_redirects,"ty"->ty_redirects,"te"->te_redirects,"ky"->ky_redirects,"rm"->rm_redirects,"pnt"->pnt_redirects,"xh"->xh_redirects,"hy"->hy_redirects,"tyv"->tyv_redirects,"tet"->tet_redirects,"la"->la_redirects,"rmy"->rmy_redirects,"xmf"->xmf_redirects,"tg"->tg_redirects,"hz"->hz_redirects,"udm"->udm_redirects,"lad"->lad_redirects,"rn"->rn_redirects,"th"->th_redirects,"yi"->yi_redirects,"ia"->ia_redirects,"ug"->ug_redirects,"lb"->lb_redirects,"ro"->ro_redirects,"ti"->ti_redirects,"yo"->yo_redirects,"id"->id_redirects,"uk"->uk_redirects,"tk"->tk_redirects,"lbe"->lbe_redirects,"roa-rup"->roa_rup_redirects,"za"->za_redirects,"ie"->ie_redirects,"ur"->ur_redirects,"lez"->lez_redirects,"tl"->tl_redirects,"roa-tara"->roa_tara_redirects,"zea"->zea_redirects,"uz"->uz_redirects,"zh"->zh_redirects,"ve"->ve_redirects,"zh-classical"->zh_classical_redirects,"vec"->vec_redirects,"zh-min-nan"->zh_min_nan_redirects,"vep"->vep_redirects,"zh-yue"->zh_yue_redirects,"vi"->vi_redirects,"zu"->zu_redirects,"jp"->ja_redirects,"cz"->cs_redirects,"minnan"->zh_min_nan_redirects,"dk"->da_redirects,"epo"->eo_redirects,"nan"->zh_min_nan_redirects,"nb"->no_redirects,"zh-cfr"->zh_min_nan_redirects)
    private def bxr_redirects = Set("#перенаправление","#перенапр","#REDIRECT")
    private def am_redirects = Set("#REDIRECT")
    private def lg_redirects = Set("#REDIRECT")
    private def mappings_redirects = Set("#REDIRECT")
    private def ig_redirects = Set("#KÚFÙ","#REDIRECT")
    private def fa_redirects = Set("#تغییر_مسیر","#تغییرمسیر","#REDIRECT")
    private def ay_redirects = Set("#REDIRECCIÓN","#REDIRECCION","#REDIRECT")
    private def ru_redirects = Set("#перенаправление","#перенапр","#REDIRECT")
    private def commons_redirects = Set("#REDIRECT")
    private def ca_redirects = Set("#REDIRECT")
    private def li_redirects = Set("#DOORVERWIJZING","#REDIRECT")
    private def az_redirects = Set("#İSTİQAMƏTLƏNDİRMƏ","#İSTİQAMƏTLƏNDİR","#REDIRECT")
    private def ii_redirects = Set("#重定向","#REDIRECT")
    private def ff_redirects = Set("#REDIRECTION","#REDIRECT")
    private def an_redirects = Set("#ENDRECERA","#REENDRECERA","#REDIRECCIÓN","#REDIRECCION","#REDIRECT")
    private def wikidata_redirects = Set("#REDIRECT")
    private def rue_redirects = Set("#ПЕРЕНАПРАВЛЕННЯ","#ПЕРЕНАПР","#перенапр","#перенаправление","#REDIRECT")
    private def lij_redirects = Set("#RINVIA","#RINVIO","#RIMANDO","#REDIRECT")
    private def cbk_zam_redirects = Set("#REDIRECCIÓN","#REDIRECCION","#REDIRECT")
    private def ba_redirects = Set("#перенаправление","#перенапр","#REDIRECT")
    private def ik_redirects = Set("#REDIRECT")
    private def aa_redirects = Set("#REDIRECT")
    private def ang_redirects = Set("#REDIRECT")
    private def fi_redirects = Set("#OHJAUS","#UUDELLEENOHJAUS","#REDIRECT")
    private def rw_redirects = Set("#REDIRECT")
    private def lmo_redirects = Set("#RINVIA","#RINVIO","#RIMANDO","#REDIRECT")
    private def cdo_redirects = Set("#重定向","#REDIRECT")
    private def bar_redirects = Set("#WEITERLEITUNG","#REDIRECT")
    private def ilo_redirects = Set("#REDIRECT")
    private def ab_redirects = Set("#перенаправление","#перенапр","#REDIRECT")
    private def fiu_vro_redirects = Set("#saadaq","#suuna","#REDIRECT")
    private def ar_redirects = Set("#تحويل","#REDIRECT")
    private def ln_redirects = Set("#REDIRECTION","#REDIRECT")
    private def sa_redirects = Set("#पुनर्निदेशन","#अनुप्रेषित","#REDIRECT")
    private def ce_redirects = Set("#дlасахьажайар'","'#хьажайо'","'#REDIRECT","#перенаправление","#перенапр","#REDIRECT")
    private def bat_smg_redirects = Set("#PERADRESAVIMAS","#REDIRECT")
    private def io_redirects = Set("#REDIRECT")
    private def ace_redirects = Set("#ALIH","#REDIRECT")
    private def arc_redirects = Set("#ܨܘܝܒܐ","#REDIRECT")
    private def fj_redirects = Set("#REDIRECT")
    private def lo_redirects = Set("#REDIRECT")
    private def sah_redirects = Set("#перенаправление","#перенапр","#REDIRECT")
    private def ceb_redirects = Set("#REDIRECT")
    private def af_redirects = Set("#AANSTUUR","#REDIRECT")
    private def is_redirects = Set("#tilvísun","#TILVÍSUN","#REDIRECT")
    private def bcl_redirects = Set("#REDIRECT")
    private def arz_redirects = Set("#تحويل","#REDIRECT")
    private def fo_redirects = Set("#REDIRECT")
    private def lt_redirects = Set("#PERADRESAVIMAS","#REDIRECT")
    private def sc_redirects = Set("#REDIRECT")
    private def ch_redirects = Set("#REDIRECT")
    private def ak_redirects = Set("#REDIRECT")
    private def as_redirects = Set("#REDIRECT")
    private def be_redirects = Set("#REDIRECT")
    private def it_redirects = Set("#RINVIA","#RINVIO","#RIMANDO","#REDIRECT")
    private def fr_redirects = Set("#REDIRECTION","#REDIRECT")
    private def ltg_redirects = Set("#REDIRECT")
    private def scn_redirects = Set("#RINVIA","#RINVIO","#RIMANDO","#REDIRECT")
    private def cho_redirects = Set("#REDIRECT")
    private def ast_redirects = Set("#REDIRECT")
    private def iu_redirects = Set("#REDIRECT")
    private def be_x_old_redirects = Set("#перанакіраваньне","#REDIRECT")
    private def als_redirects = Set("#WEITERLEITUNG","#REDIRECT")
    private def lv_redirects = Set("#REDIRECT")
    private def frp_redirects = Set("#REDIRÈCCION","#REDIRECTION","#REDIRECT")
    private def chr_redirects = Set("#REDIRECT")
    private def av_redirects = Set("#перенаправление","#перенапр","#REDIRECT")
    private def sco_redirects = Set("#REDIRECT")
    private def bg_redirects = Set("#пренасочване","#виж","#REDIRECT")
    private def ja_redirects = Set("#転送","#リダイレクト","＃転送","＃リダイレクト","#REDIRECT")
    private def mai_redirects = Set("#अनुप्रेषित","#REDIRECT")
    private def frr_redirects = Set("#WEITERLEITUNG","#REDIRECT")
    private def bh_redirects = Set("#REDIRECT")
    private def chy_redirects = Set("#REDIRECT")
    private def nds_nl_redirects = Set("#DEURVERWIEZING","#DUURVERWIEZING","#DOORVERWIJZING","#REDIRECT")
    private def sd_redirects = Set("#چوريو","#REDIRECT")
    private def bo_redirects = Set("#REDIRECT")
    private def jbo_redirects = Set("#REDIRECT")
    private def ckb_redirects = Set("#REDIRECT")
    private def map_bms_redirects = Set("#ALIH","#REDIRECT")
    private def fur_redirects = Set("#RINVIA","#RINVIO","#RIMANDO","#REDIRECT")
    private def bi_redirects = Set("#REDIRECT")
    private def ne_redirects = Set("#REDIRECT")
    private def bpy_redirects = Set("#REDIRECT")
    private def se_redirects = Set("#STIVREN","#OĐĐASITSTIVREN","#REDIRECT")
    private def mdf_redirects = Set("#REDIRECT")
    private def jv_redirects = Set("#ALIH","#REDIRECT")
    private def fy_redirects = Set("#REDIRECT")
    private def co_redirects = Set("#RINVIA","#RINVIO","#RIMANDO","#REDIRECT")
    private def bjn_redirects = Set("#ALIH","#REDIRECT")
    private def new_redirects = Set("#REDIRECT")
    private def br_redirects = Set("#ADKAS","#REDIRECT")
    private def sg_redirects = Set("#REDIRECTION","#REDIRECT")
    private def mg_redirects = Set("#FIHODINANA","#REDIRECTION","#REDIRECT")
    private def ka_redirects = Set("#გადამისამართება","#REDIRECT")
    private def ga_redirects = Set("#athsheoladh","#REDIRECT")
    private def cr_redirects = Set("#REDIRECT")
    private def bm_redirects = Set("#REDIRECTION","#REDIRECT")
    private def ng_redirects = Set("#REDIRECT")
    private def bs_redirects = Set("#PREUSMJERI","#REDIRECT")
    private def sh_redirects = Set("#PREUSMJERI","#PREUSMERI","#REDIRECT")
    private def mh_redirects = Set("#REDIRECT")
    private def kaa_redirects = Set("#AÝDAW","#АЙДАУ","#REDIRECT")
    private def crh_redirects = Set("#REDIRECT")
    private def bn_redirects = Set("#REDIRECT")
    private def gag_redirects = Set("#YÖNNENDİRMÄKLER","#YÖNNENDİR","#YÖNNENDİRMÄ","#YÖNLENDİRME","#YÖNLENDİR","#REDIRECT")
    private def nl_redirects = Set("#DOORVERWIJZING","#REDIRECT")
    private def bug_redirects = Set("#ALIH","#REDIRECT")
    private def si_redirects = Set("#යළියොමුව","#REDIRECT")
    private def mhr_redirects = Set("#перенаправление","#перенапр","#REDIRECT")
    private def kab_redirects = Set("#REDIRECT")
    private def cs_redirects = Set("#PŘESMĚRUJ","#REDIRECT")
    private def gan_redirects = Set("#重定向","#REDIRECT")
    private def cy_redirects = Set("#ail-cyfeirio","#ailgyfeirio","#REDIRECT")
    private def mk_redirects = Set("#пренасочување","#види","#Пренасочување","#ПРЕНАСОЧУВАЊЕ","#REDIRECT")
    private def nn_redirects = Set("#omdiriger","#REDIRECT")
    private def simple_redirects = Set("#REDIRECT")
    private def mi_redirects = Set("#REDIRECT")
    private def kbd_redirects = Set("#REDIRECT")
    private def gd_redirects = Set("#REDIRECT")
    private def ml_redirects = Set("#തിരിച്ചുവിടുക","#തിരിച്ചുവിടൽ","#REDIRECT")
    private def csb_redirects = Set("#PATRZ","#PRZEKIERUJ","#TAM","#REDIRECT")
    private def no_redirects = Set("#OMDIRIGERING","#REDIRECT")
    private def da_redirects = Set("#REDIRECT")
    private def min_redirects = Set("#ALIAH","#ALIH","#REDIRECT")
    private def kg_redirects = Set("#REDIRECT")
    private def sk_redirects = Set("#presmeruj","#REDIRECT")
    private def mn_redirects = Set("#REDIRECT")
    private def gl_redirects = Set("#REDIRECCIÓN","#REDIRECIONAMENTO","#REDIRECT")
    private def nov_redirects = Set("#REDIRECT")
    private def cu_redirects = Set("#ПРѢНАПРАВЛЄНИѤ","#REDIRECT")
    private def ki_redirects = Set("#REDIRECT")
    private def de_redirects = Set("#WEITERLEITUNG","#REDIRECT")
    private def mwl_redirects = Set("#ANCAMINAR","#REDIRECIONAMENTO","#REDIRECT")
    private def sl_redirects = Set("#PREUSMERITEV","#REDIRECT")
    private def mo_redirects = Set("#REDIRECTEAZA","#REDIRECT")
    private def glk_redirects = Set("#تغییر_مسیر","#تغییرمسیر","#REDIRECT")
    private def nrm_redirects = Set("#REDIRECT")
    private def cv_redirects = Set("#перенаправление","#перенапр","#REDIRECT")
    private def diq_redirects = Set("#HETENAYIŞ","#REDIRECT")
    private def kj_redirects = Set("#REDIRECT")
    private def my_redirects = Set("#REDIRECT")
    private def mr_redirects = Set("#पुनर्निर्देशन","#पुर्ननिर्देशन","#REDIRECT")
    private def gn_redirects = Set("#REDIRECCIÓN","#REDIRECCION","#REDIRECT")
    private def nso_redirects = Set("#REDIRECT")
    private def ee_redirects = Set("#REDIRECT")
    private def sm_redirects = Set("#REDIRECT")
    private def dsb_redirects = Set("#WEITERLEITUNG","#REDIRECT")
    private def kk_redirects = Set("#АЙДАУ","#REDIRECT")
    private def mrj_redirects = Set("#перенаправление","#перенапр","#REDIRECT")
    private def myv_redirects = Set("#перенаправление","#перенапр","#REDIRECT")
    private def nv_redirects = Set("#REDIRECT")
    private def got_redirects = Set("#REDIRECT")
    private def sn_redirects = Set("#REDIRECT")
    private def el_redirects = Set("#ΑΝΑΚΑΤΕΥΘΥΝΣΗ","#REDIRECT")
    private def dv_redirects = Set("#REDIRECT")
    private def kl_redirects = Set("#REDIRECT")
    private def mzn_redirects = Set("#بور","#تغییرمسیر","#تغییر_مسیر","#REDIRECT")
    private def ny_redirects = Set("#REDIRECT")
    private def ms_redirects = Set("#LENCONG","#REDIRECT")
    private def gu_redirects = Set("#REDIRECT")
    private def so_redirects = Set("#REDIRECT")
    private def eml_redirects = Set("#RINVIA","#RINVIO","#RIMANDO","#REDIRECT")
    private def dz_redirects = Set("#REDIRECT")
    private def km_redirects = Set("#បញ្ជូនបន្ត","#ប្ដូរទីតាំងទៅ","#ប្តូរទីតាំងទៅ","#ប្ដូរទីតាំង","#ប្តូរទីតាំង","#ប្ដូរចំណងជើង","#REDIRECT")
    private def na_redirects = Set("#REDIRECT")
    private def gv_redirects = Set("#REDIRECT")
    private def mt_redirects = Set("#RINDIRIZZA","#REDIRECT")
    private def oc_redirects = Set("#REDIRECCION","#REDIRECT")
    private def sq_redirects = Set("#RIDREJTO","#REDIRECT")
    private def en_redirects = Set("#REDIRECT")
    private def es_redirects = Set("#REDIRECCIÓN","#REDIRECCION","#REDIRECT")
    private def kn_redirects = Set("#REDIRECT")
    private def mus_redirects = Set("#REDIRECT")
    private def nah_redirects = Set("#REDIRECCIÓN","#REDIRECCION","#REDIRECT")
    private def ha_redirects = Set("#REDIRECT")
    private def sr_redirects = Set("#Преусмери","#преусмери","#ПРЕУСМЕРИ","#Преусмјери","#преусмјери","#ПРЕУСМЈЕРИ","#redirect","#REDIRECT")
    private def om_redirects = Set("#REDIRECT")
    private def eo_redirects = Set("#ALIDIREKTI","#ALIDIREKTU","#AL","#REDIRECT")
    private def ko_redirects = Set("#넘겨주기","#REDIRECT")
    private def et_redirects = Set("#suuna","#REDIRECT")
    private def hak_redirects = Set("#重定向","#REDIRECT")
    private def nap_redirects = Set("#RINVIA","#RINVIO","#RIMANDO","#REDIRECT")
    private def srn_redirects = Set("#STIR","#DOORVERWIJZING","#REDIRECT")
    private def or_redirects = Set("#ଲେଉଟାଣି","#REDIRECT")
    private def koi_redirects = Set("#перенаправление","#перенапр","#REDIRECT")
    private def nds_redirects = Set("#wiederleiden","#WEITERLEITUNG","#REDIRECT")
    private def haw_redirects = Set("#REDIRECT")
    private def eu_redirects = Set("#BIRZUZENDU","#REDIRECT")
    private def os_redirects = Set("#ÆРВИТÆН","#ÆРВЫСТ","#РАРВЫСТ","#перенаправление","#перенапр","#REDIRECT")
    private def tn_redirects = Set("#REDIRECT")
    private def ss_redirects = Set("#REDIRECT")
    private def kr_redirects = Set("#REDIRECT")
    private def pap_redirects = Set("#REDIRECT")
    private def ext_redirects = Set("#REDIRECT")
    private def he_redirects = Set("#הפניה","#REDIRECT")
    private def to_redirects = Set("#REDIRECT")
    private def st_redirects = Set("#REDIRECT")
    private def pa_redirects = Set("#ਰੀਡਿਰੈਕਟ","#REDIRECT")
    private def pcd_redirects = Set("#REDIRECTION","#REDIRECT")
    private def hi_redirects = Set("#अनुप्रेषित","#REDIRECT")
    private def krc_redirects = Set("#джибериу","#редирект","#перенаправление","#перенапр","#REDIRECT")
    private def vls_redirects = Set("#DOORVERWIJZING","#REDIRECT")
    private def tpi_redirects = Set("#REDIRECT")
    private def stq_redirects = Set("#WEITERLEITUNG","#REDIRECT")
    private def pag_redirects = Set("#REDIRECT")
    private def pdc_redirects = Set("#WEITERLEITUNG","#REDIRECT")
    private def hif_redirects = Set("#REDIRECT")
    private def vo_redirects = Set("#REDIRECT")
    private def pam_redirects = Set("#REDIRECT")
    private def tr_redirects = Set("#YÖNLENDİRME","#YÖNLENDİR","#REDIRECT")
    private def su_redirects = Set("#ALIH","#REDIRECT")
    private def ks_redirects = Set("#REDIRECT")
    private def pfl_redirects = Set("#WEITERLEITUNG","#REDIRECT")
    private def ho_redirects = Set("#REDIRECT")
    private def wa_redirects = Set("#REDIRECTION","#REDIRECT")
    private def ts_redirects = Set("#REDIRECT")
    private def pnb_redirects = Set("#REDIRECT")
    private def ksh_redirects = Set("#ÖMLEIDE_OP","#ÖMLEIDE","#LEIDT_ÖM_OP","#ÖMLEIDUNG","#WEITERLEITUNG","#REDIRECT")
    private def sv_redirects = Set("#OMDIRIGERING","#REDIRECT")
    private def hr_redirects = Set("#PREUSMJERI","#REDIRECT")
    private def war_redirects = Set("#REDIRECT")
    private def pi_redirects = Set("#REDIRECT")
    private def sw_redirects = Set("#REDIRECT")
    private def tt_redirects = Set("#ЮНӘЛТҮ","#перенаправление","#перенапр","#REDIRECT")
    private def ps_redirects = Set("#REDIRECT")
    private def ku_redirects = Set("#BERALÎKIRIN","#REDIRECT")
    private def hsb_redirects = Set("#WEITERLEITUNG","#REDIRECT")
    private def wo_redirects = Set("#REDIRECTION","#REDIRECT")
    private def pih_redirects = Set("#REDIRECT")
    private def szl_redirects = Set("#PATRZ","#PRZEKIERUJ","#TAM","#REDIRECT")
    private def tum_redirects = Set("#REDIRECT")
    private def pt_redirects = Set("#REDIRECIONAMENTO","#REDIRECT")
    private def kv_redirects = Set("#перенаправление","#перенапр","#REDIRECT")
    private def ht_redirects = Set("#REDIRECTION","#REDIRECT")
    private def wuu_redirects = Set("#重定向","#REDIRECT")
    private def pl_redirects = Set("#PATRZ","#PRZEKIERUJ","#TAM","#REDIRECT")
    private def ta_redirects = Set("#வழிமாற்று","#REDIRECT")
    private def tw_redirects = Set("#REDIRECT")
    private def qu_redirects = Set("#PUSAPUNA","#REDIRECCIÓN","#REDIRECCION","#REDIRECT")
    private def kw_redirects = Set("#DASKEDYANS","#REDIRECT")
    private def xal_redirects = Set("#перенаправление","#перенапр","#REDIRECT")
    private def hu_redirects = Set("#ÁTIRÁNYÍTÁS","#REDIRECT")
    private def pms_redirects = Set("#RINVIA","#RINVIO","#RIMANDO","#REDIRECT")
    private def ty_redirects = Set("#REDIRECTION","#REDIRECT")
    private def te_redirects = Set("#దారిమార్పు","#REDIRECT")
    private def ky_redirects = Set("#REDIRECT")
    private def rm_redirects = Set("#RENVIAMENT","#REDIRECT")
    private def pnt_redirects = Set("#REDIRECT")
    private def xh_redirects = Set("#REDIRECT")
    private def hy_redirects = Set("#ՎԵՐԱՀՂՈՒՄ","#REDIRECT")
    private def tyv_redirects = Set("#көжүрүлге","#ШИГЛЕДИР","#перенаправление","#перенапр","#REDIRECT")
    private def tet_redirects = Set("#REDIRECT")
    private def la_redirects = Set("#REDIRECT")
    private def rmy_redirects = Set("#REDIRECTEAZA","#REDIRECT")
    private def xmf_redirects = Set("#გადამისამართება","#REDIRECT")
    private def tg_redirects = Set("#REDIRECT")
    private def hz_redirects = Set("#REDIRECT")
    private def udm_redirects = Set("#перенаправление","#перенапр","#REDIRECT")
    private def lad_redirects = Set("#DIRIJAR","#DIRECCIÓN","#REDIRECCIÓN","#REDIRECCION","#REDIRECT")
    private def rn_redirects = Set("#REDIRECT")
    private def th_redirects = Set("#เปลี่ยนทาง","#REDIRECT")
    private def yi_redirects = Set("#ווייטערפירן","#הפניה","#REDIRECT")
    private def ia_redirects = Set("#REDIRECT")
    private def ug_redirects = Set("#REDIRECT")
    private def lb_redirects = Set("#VIRULEEDUNG","#WEITERLEITUNG","#REDIRECT")
    private def ro_redirects = Set("#REDIRECTEAZA","#REDIRECT")
    private def ti_redirects = Set("#REDIRECT")
    private def yo_redirects = Set("#REDIRECT")
    private def id_redirects = Set("#ALIH","#REDIRECT")
    private def uk_redirects = Set("#ПЕРЕНАПРАВЛЕННЯ","#ПЕРЕНАПР","#перенапр","#перенаправление","#REDIRECT")
    private def tk_redirects = Set("#REDIRECT")
    private def lbe_redirects = Set("#перенаправление","#перенапр","#REDIRECT")
    private def roa_rup_redirects = Set("#REDIRECT")
    private def za_redirects = Set("#重定向","#REDIRECT")
    private def ie_redirects = Set("#REDIRECT")
    private def ur_redirects = Set("#رجوع_مکرر","#REDIRECT")
    private def lez_redirects = Set("#перенаправление","#перенапр","#REDIRECT")
    private def tl_redirects = Set("#REDIRECT")
    private def roa_tara_redirects = Set("#REDIRECT")
    private def zea_redirects = Set("#DOORVERWIJZING","#REDIRECT")
    private def uz_redirects = Set("#YOʻNALTIRISH","#YONALTIRISH","#REDIRECT")
    private def zh_redirects = Set("#重定向","#REDIRECT")
    private def ve_redirects = Set("#REDIRECT")
    private def zh_classical_redirects = Set("#REDIRECT")
    private def vec_redirects = Set("#VARDA","#RINVIA","#RINVIO","#RIMANDO","#REDIRECT")
    private def zh_min_nan_redirects = Set("#重定向","#REDIRECT")
    private def vep_redirects = Set("#suuna","#REDIRECT")
    private def zh_yue_redirects = Set("#REDIRECT")
    private def vi_redirects = Set("#đổi","#REDIRECT")
    private def zu_redirects = Set("#REDIRECT")



    def apply(language : Language) = map(language.wikiCode)
}
