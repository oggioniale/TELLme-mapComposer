#urlexample="http://tellmehub.get-it.it/api/layers/?keywords__slug__iexact=concept_77&extent=101.46972656250001,15.178180945596363,-95.69091796875001,21.43261686447735"

require(jqr); require(httr); require(dplyr); require(jsonlite); require(dtplyr); require(purrr)
TELLMEHUB_URL<-'http://tellmehub.get-it.it/'


####### functions for retrieving objects from get-it API in json format -----
getApiObjectsJson<-function(url=TELLMEHUB_URL,object_type='layers',id='',query=''){
  if(query!='') query<-paste0('?',query)
  
  url<-paste0(url,'api/',object_type,'/',id,query)
  cat(paste0('obtaining json from url: ',url))
  export<-httr::GET(url = url)
  jsonunparsed<-httr::content(export,"text")
  #cat('...done')
  return(jsonunparsed)
}

# shorcut for layers
getLayersJson<-function(url=TELLMEHUB_URL,id=''){
  getApiObjectsJson(url=url,object_type='layers',id=id)
}

##### prepare datatables ----
# use jq to parse json objects. The result is converted into a dataframe 
# and then in a lazy_datatable (package dtplyr)

j2dt<-function(x){
  textConnection(x) %>% jsonlite::stream_in(simplifyDataFrame=TRUE) %>% 
    dtplyr::lazy_dt()
}
j2df<-function(x){
  textConnection(x) %>% jsonlite::stream_in(simplifyDataFrame=TRUE)
}
# LAYERS
dt_getit_AllLayers <- getLayersJson() %>% 
    jqr::jq('.objects[] | {
            id: .id, 
            uri_api:.resource_uri, 
            name:.name, 
            title:.title, 
            typename: .typename, 
            alternate:.alternate}') %>% 
    j2dt()

# KEYWORDS, i.e. Hierarchical Keywords relevant to TELLme in get-it: 
#   they have slugs beginnning with:
#   concepts_, keywords_, scale_
dt_getit_Keywords<-
  getApiObjectsJson(object_type = 'keywords') %>% 
  jqr::jq('.objects[]|
          select((.slug|test("scale_") or test("concept_") or test("keyword_"))) |
          {
            API_id:.id,
            API_type: "keywords",
            name:.name,
            slug:.slug,
            resource_uri:.resource_uri,
            tellme_type: (if (.slug|contains("concept"))==true then "concept" elif (.slug|contains("scale"))==true then "scale" else "keyword" end)
          } ') %>% 
  j2dt()

##### Prepare datatables with many to many relations ----
# tables will have denormalized versions (in length and width)
# in order to store and quickly retrieve all infos about e.g. one keyword when looking for 
# a layer: a single row representing the relation among a layer and a keyword will have
# all relevant infos both for the keyword and for the layer.

# Development note: we are in need for a loop through the ids e.g. of all layers, which
# could be retrieved by the list of layers objects in the API.
# Some detail of a layer, and in particular its being tagged with a specific keyword,
# is contained in a detail API endpoint (/api/layers/{layer_id}).
# So, we need to loop the ids and add the adjoint infos to the layer.
# We will expolit purrr::map family of functions, and in particular map_dfr which is
# able to make dataframe row-binding to its results.
# In these cases we will need a helper function to be passed to purrr:map_dfr.
#
# [1. ciclare nel modo migliore possibile sugli id nella tabella generale dei layers]
# [2. far crescere la nuova tabella m2m via via che interrogo get-it per ogni layer?]
## require(purrr)
# funzione di appoggio per ciclare 
helper.keywordsPerLayer_df<-function(x){
  print(x)
  layer_json<-getLayersJson(url=TELLMEHUB_URL, id=x)
  dftemp<-layer_json %>% 
    jqr::jq('if ([.keywords[]]|length==0) 
            then {
              id_api_keyword:"NULL",
              layer_store:.store,layer_storeType:.storeType
            } 
            else {
              id_api_keyword:.keywords[],
              layer_store:.store,layer_storeType:.storeType
            }
            end
    ') %>% 
    j2df()
    #textConnection() %>% jsonlite::stream_in(simplifyDataFrame=TRUE)
  print(dftemp)
  return(dftemp)
}

#tmp_df_in<-dt_getit_AllLayers %>% #dplyr::filter(id<500 & id>470) %>% 
#  dplyr::select(id) %>% as.data.frame() %>% .[,1]
#%>%  as.list() %>% 
m2m_LayersKeywords<-
  dt_getit_AllLayers %>% #dplyr::filter(id<500 & id>470) %>% 
  dplyr::select(id) %>% as.data.frame() %>% .[,1] %>% 
  purrr::map_dfr(function(.x) { #dfr mi serve per accodare righe quando più di una
    dd<-helper.keywordsPerLayer_df(.x) # restituisce un dataframe con tante righe quante le keywords associate al layer
    if(nrow(dd)==0) dd=NA #va corretto comportamento per layer senza keywords
    return(data.frame(id_layer = .x, 
                    dd))}
  ) %>% as_tibble()
m2m_LayersKeywords<-m2m_LayersKeywords %>% dtplyr::lazy_dt()
# analizzo layer con piu di una keyword
#m2m_LayersKeywords %>% group_by(id_layer) %>% summarise(n=n()) %>% dplyr::filter(n>=2) %>% as.data.frame()
#m2m_LayersKeywords %>% dplyr::filter(id_api_keyword %in% (dt_getit_Keywords %>% filter(tellme_type=="concept") %>% dplyr::select(resource_uri)))
#dt_getit_Keywords %>% filter(tellme_type=="concept") 

# ottengo nuova m2m ripulita da keywords che non sono related concepts tellme
# id_layer id_keyword concept_slug conceptId concept 
m2m_Layers_ConceptKeywords <- m2m_LayersKeywords %>% inner_join(
  (dt_getit_Keywords %>% filter(tellme_type=="concept")), 
  by=c(id_api_keyword="resource_uri")) %>% 
  dplyr::select(id_layer, id_keyword=API_id, concept_slug=slug, conceptId=slug, concept=name) %>% 
  mutate(conceptId=gsub("concept_","",concept_slug))

m2m_denorm_Layers_ConceptKeywords<-m2m_Layers_ConceptKeywords %>% 
  inner_join(dt_getit_AllLayers %>% rename(id_layer=id, layer_uri=uri_api, layer_name=name, layer_title=title, layer_typename=typename, layer_alternate=alternate))
####### NOTE ----

# si puo fare ricerca layers data keyword slug come qui sopra, possibile anche dare extent (bbx)

# l'elenco di tutti i layer è all'url
#   http://tellmehub.get-it.it/api/layers
# il dettaglio di ogni layer è in url del tipo
#   http://tellmehub.get-it.it/api/layers/{layer_id}
# attributi importanti per ogni layer:
#
#   resource_uri:	"/api/layers/450/"

# invece per cercare layer per nome:
#   http://tellmehub.get-it.it/api/layers/?name__iexact=wwf_global_lakes_and_wetlands_newEPSG
# da qui posso, recuperando resource_uri, passare poi a e.g.
#   http://tellmehub.get-it.it/api/layers/397
# che contiene le keywords (che vengono esposte però per id delle api e non per slug)

# l'elenco delle keywords è all'url
#   http://tellmehub.get-it.it/api/keywords/
# e il loro dettaglio è allo stesso seguito da {keyword_id}
#   http://tellmehub.get-it.it/api/keywords/{keyword_id}

# elenco mappe:
#   http://tellmehub.get-it.it/api/maps/
# objects[] title
# ricerca mappe di data categoria (protocollo nel mio caso)
#  http://tellmehub.get-it.it/api/maps/?category__identifier__iexact=protocol_1
#
# e anche con la scala (messa come keyword):
#   http://tellmehub.get-it.it/api/maps/?category__identifier__iexact=protocol_1&keywords__slug__iexact=scale_XL
#
if(FALSE){ # tabella delle sole mappe in realtà non mi serve a granché
dt_Maps<-getApiObjectsJson(object_type = "maps") %>% 
  jqr::jq('.objects[]|{
          map_id:.id,
          map_title:.title,
          map_srid:.srid,
          map_owner_username:.owner__username
          }') %>% j2dt()
}
# all'interno di ogni oggetto mappa vanno cercati i 
# layers[] tali che il loro group!=background
# ne deve essere preso il .name
# costruisco direttamente many 2 many Mappe-Layers.
# la chiamo denorm perché info singola mappa sono ripetute su più righe
m2m_denorm_MapsLayers_0 <- getApiObjectsJson(object_type = "maps")  %>% 
  jqr::jq(paste0('.objects[]|',  # SE VOLESSI LA SELEZIONE qui FAREI: select(.id==',map_id,') | 
        '{
          map_id:.id,
          map_layer__name_group_local:(.layers[]|select(.group!="background")|[.name,.group,.local]),
          map_title:.title,
          map_srid:.srid,
          map_owner_username:.owner__username
        } | {
          map_id:.map_id,
          layer_typename:.map_layer__name_group_local[0],
          layer_local:.map_layer__name_group_local[2],
          map_title:.map_title,
          map_srid:.map_srid,
          map_owner_username:.map_owner_username
        }') ) %>% j2dt()

# funzione di appoggio per ciclare su ogni singola mappa. Mi serve per estrarre keywords (contiene la scala)
# e category_identifier (contiene il fatto che sia o meno un Protocol, e quale)
# NOTA BENE: in uscita ho tante righe quante sono le keywords associate alle mappe.
# anche se in linea di massima mi aspetto una sola keyword (=scala) per ogni mappa
#                      [map_id,   keyword,              category_identifier,   category_uri,         category_description,   uri,                     title,            srid,         projection,   owner_username  ]
# id_mappa -> keyword1 [id_mappa, "/api/keywords/291/", "protocol_1",          "/api/categories/20", "green gray protocol",  "/api/maps/{id_mappa}",  "GG Guadalajara", "EPSG:4326",  "EPSG:3857",  "admin"         ]
#             keywordN [id_mappa, "/api/keywords/292/"  "protocol_1"    .....  ]
#
helper.get_map_details_df<-function(x){
  print(x)
  dftemp<-getApiObjectsJson(object_type = "maps", id=x) %>% 
    jqr::jq('.| {
          map_id:.id,
          keywords:.keywords[],
          category_identifier:.category.identifier,
          category_uri:.category.resource_uri,
          category_description:.category.description,
          uri:.resource_uri,
          title:.title,
          srid:.srid,
          projection:.projection,
          owner_username:.owner.username
          }') %>% 
    j2df()
  
  #join(dftemp,dftemp_2,on=c(map_id="map_id"))
  print(dftemp)
  return(dftemp)
  #dftemp[1,]
}

#tmp_df_in<-dt_getit_AllLayers %>% #dplyr::filter(id<500 & id>470) %>% 
#  dplyr::select(id) %>% as.data.frame() %>% .[,1]
#%>%  as.list() %>% 
dt_MapsDetails<-
  m2m_denorm_MapsLayers_0 %>% 
  dplyr::select(map_id) %>% unique() %>% #ottengo lista degli id delle mappe dalla tabella m2m
  as.data.frame() %>% .[,1] %>% 
  purrr::map_dfr(function(.x) { #dfr mi serve per accodare righe quando più di una
    dd<-helper.get_map_details_df(.x) # restituisce un dataframe con tante righe quante le keywords associate al layer
    if(nrow(dd)==0) dd=NA #va corretto comportamento per mappe senza keywords
    return(data.frame(map_id = .x, 
                      dd))}
  ) %>% as_tibble() %>% dtplyr::lazy_dt()


#select(map_id,keywords, category_identifier)
# ora devo aggiungere tutte le info di dettaglio di ogni mappa
# alla tabella m2m denormalizzata tra mappe e layers.
# Avrei un problema se ci fossero mappa con più di una keyword!!!
# ossia la colonna n della query seguente deve essere al più 1
#    dt_MapsDetails %>% group_by(map_id) %>% count
# in ogni caso, per completare info su mappe a me utili, 
# devo aggiungere anche info sulla keyword che nella tabella appena creata è
# solo /api/keyword/.. mentre a me serve lo slug che mi permette di capire
# la scala tellme attribuita alla mappa
dt_MapsDetails_denorm<-dt_MapsDetails %>% rename(keyword_uri=keywords) %>% 
  left_join(dt_getit_Keywords %>% 
              dplyr::select(keyword_uri=resource_uri,keyword_slug=slug, keyword_type=tellme_type), by(keyword_uri)) %>% 
  select(map_id, title, category_identifier, keyword_slug, keyword_type, category_description) %>% 
  filter(keyword_type=="scale")
  
# denorm_m2m_Layers_ConceptKeywords %>% group_by(id_layer) %>% count %>% filter(n>1)
# 
# denorm_m2m_Layers_ConceptKeywords %>% filter(id_layer %in% c(371,391,392))
# 
# m2m_denorm_MapsLayers_0 %>% left_join(denorm_m2m_Layers_ConceptKeywords)
# View(m2m_denorm_MapsLayers_0 %>% left_join(dt_MapsDetails_denorm) %>% as_tibble())
#View(dt_MapsDetails_denorm %>% as_tibble())

# ho un:  "layer_name": "WDPA_Feb2019-shapefile-polygons",
# quale nome in dt_layers devo usare?
dt_AllLayers %>% filter(typename=="WDPA_Feb2019-shapefile-polygons")
dt_AllLayers %>% filter(typename=="airports")
dt_AllLayers %>% filter(alternate!=typename)

tt<-"geonode:gua_groundwater_management_2018_conagua_ground_water"
dt_AllLayers %>% filter(typename==tt)

getApiObjectsJson(object_type = 'layers', id=380) %>% jqr::jq('.')
# elenco categories:
#   http://tellmehub.get-it.it/api/categories/
# objects[] identifier==protocol_N resource_uri (e.g. /api/categories/26)


dt_Styles <- getApiObjectsJson(object_type = "styles") %>% #select(.name|test("c_.*-p_.*-s_.*")) |
  jqr::jq('.objects[]| select(.sld_url|test("c_.*-p_.*-s_.*")) | {
    style_id:.id,
    name:.name,
    title:.title,
    workspace:.workspace,
    sld_url:.sld_url
  }|{style_id:.style_id,name:.name, sld_url:.sld_url}
          ') %>% j2df() %>% View()
 
# manca c_22-p_1-s_L (flood)

                
#dt_Styles<-
  getApiObjectsJson(object_type = "styles") %>% 
  jqr::jq('.objects[]|{
          map_id:.id,
          map_title:.title,
          map_srid:.srid,
          map_owner_username:.owner__username
          }') %>% j2dt()
