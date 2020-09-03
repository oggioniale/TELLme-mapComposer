require(jqr); require(httr); require(dplyr); require(jsonlite); require(dtplyr); require(purrr); require(tidyr);require(rvest)
TELLMEHUB_URL<-'http://tellmehub.get-it.it/'
source("accounts_private.R")

#printHeader<-function(x){x %>% as_tibble()%>% colnames() %>% paste(collapse=", ")}
`%notin%` <<- Negate(`%in%`)

getTELLmeHub <- function(read_online=TRUE, 
                         TELLMEHUB_URL, TELLME_HUB_USER, TELLME_HUB_PASSWORD, 
                         exportUtilsFunctions=FALSE,
                         writeJJtoDisk=FALSE, exportAdminFunctions=FALSE) {
  require(jqr); require(httr); require(dplyr); require(jsonlite); require(dtplyr); require(purrr); require(tidyr);require(rvest)
  
  self=list()
  utils=list() #if export utils, it will be appended to self
  info=list() # stores info about exported slots (which are not tables)
  
  
  # jq queries
  {
    q.ids <-".objects[].id"
    qn<-list(
      layers.ids=
        list(api="layers",     q=".objects[] | {layer_id: .id}"),
      layers=
        list(api="layers/set", q=".objects[] | {layer_id: .id,
             layer_api_uri:.resource_uri, 
             layer_name:.name, 
             layer_title:.title, 
             layer_typename: .typename, 
             layer_alternate:.alternate,
             layer_store:.store,
             layer_storeType:.storeType}",
             api_for_ids="layers", q_for_ids=q.ids),
      m2m_layersKeywords=
        list(api="layers/set",     q='.objects[] | {layer_id: .id, 
             hkeyword_id:( ((.keywords[])  |capture( "(?<id>[^/]*)/$" )) | .id | tonumber)}'),
      keywords=
        list(api="keywords",   q='.objects[] | {hkeyword_id:.id,
             hkeyword_name:.name,
             hkeyword_slug:.slug,
             hkeyword_api_uri:.resource_uri,
             hkeyword_tellme_type: (if (.slug|contains("concept"))==true then "concept" 
             elif (.slug|contains("scale"))==true then "scale" 
             elif (.slug|contains("keyword"))==true then "keyword" 
             else "other" end)
             }'),
      maps.ids=
        list(api="maps",       q='.objects[] | {map_id:.id}'),
      m2m_mapsLayers=
        list(api="maps",       q='.objects[] | {
             map_id:.id,
             map_layer__name_group_local:(.layers[]|select(.group!="background")|[.name,.local,.ows_url])
             } | {
             map_id:.map_id,
             layer_typename:.map_layer__name_group_local[0],
             layer_local:.map_layer__name_group_local[1],
             layer_ows_url:.map_layer__name_group_local[2]
             }'),
      maps=
        list(api='maps/set',   q='.objects[] | {
             map_id:.id,
             protocol_id: (( (.category.identifier//"")  | (capture("protocol_(?<id>.+)$")//{id:nan}) ) | .id | tonumber),
             category_identifier:.category.identifier, #e.g. protocol_1
             #category_uri:.category.resource_uri,  #e.g. /api/categories/20/
             #category_description:.category.description, #e.g. "Green Grey Protocol - Semantic Package"
             map_uri:.resource_uri, #e.g. "/api/maps/484/"
             map_title:.title, #e.g. "GreenGray XL Protocol 1 - Guadalajara"
             map_srid:.srid, #e.g. "EPSG:4326"
             map_projection:.projection, #e.g. "EPSG:3857"
             map_owner_username:.owner.username #e.g. "admin"
             }',
             api_for_ids="maps", q_for_ids=q.ids),
      m2m_mapsKeywords=
        list(api='maps/set',   q='.objects[] | {
             map_id:.id,
             hkeyword_id:( ((.keywords[])  |capture( "(?<id>[^/]*)/$" )) | .id | tonumber)}',
             api_for_ids="maps", q_for_ids=q.ids),
      # we will then lookup keywords and select the "scale_" slugged hierarchical keywords only
      styles=
        list(api='styles',     q='.objects[] |  {
             style_id:.id,
             #layer_API_url:.layer[],
             style_name:.name,
             style_title:.title,
             style_workspace:.workspace,
             style_sld_url:.sld_url,
             style_is_protocolStyle:(.sld_url|test("c_.*-p_.*-s_.*")),
             s: (.sld_url | capture("c_(?<c>[^-]*)-p_(?<p>[^-]*)-s_(?<s>[^-]*).sld")//{c:"",p:"",s:""})
             }|{
             style_id:.style_id,
             style_name:.style_name, 
             style_title:.style_title,
             style_workspace:.style_workspace,
             style_sld_url:.style_sld_url,
             style_is_protocolStyle:.style_is_protocolStyle,
             style_concept: .s.c, 
             style_protocol: .s.p, 
             style_scale:.s.s
             }'),
      m2m_stylesLayers=
        list(api='styles',      q='.objects[] | {
             style_id:.id,
             layer_id:( ((.layer[])  |capture( "(?<id>[^/]*)/$" )) | .id | tonumber)
        }') 
      # NOTE: only tellme-related styles are currently retrieved (based on naming convention)
      # geonode APIs are exposing more than one style instance (why? check in the more recent geonode version)
    )
  } # jq queries
  
  ###### internal variables #####
  {
    ####### code generator (qn must be set) #####
    ## produce statements for internal variabiles, the code must be put at the beginning of the internal variables section
    #
    # for (n in names(qn)){
    #   prefix=""
    #   if(!startsWith(n,"m2m_")) prefix="dt_"
    #   cat(sprintf("%s%s <- NULL\n",prefix,n))
    # }
    dt_layers.ids <- NULL
    dt_layers <- NULL
    m2m_layersKeywords <- NULL
    dt_keywords <- NULL
    dt_maps.ids <- NULL
    m2m_mapsLayers <- NULL
    dt_maps <- NULL
    m2m_mapsKeywords <- NULL
    dt_styles <- NULL
    m2m_stylesLayers <- NULL
    
    ## end of auto-generated code
    dt_services <- NULL
    
    # any other internal variables
    #getlayersForBeans=NULL # function to retrieve, given a set of tellme concepts, (a geographic area) and a tellme scale, the corresponding available layers within the hub.
    authsession <- NULL
  }# internal variables
  
  
  # internal utilities ####
  {
    j2dt<-function(x){
      textConnection(x) %>% jsonlite::stream_in(simplifyDataFrame=TRUE) %>% 
        dtplyr::lazy_dt()
    }
    j2df<-function(x){
      textConnection(x) %>% jsonlite::stream_in(simplifyDataFrame=TRUE)
    }
    
    ####### functions for retrieving objects from get-it API in json format -----
    getApiObjectsJson<-function(object_type,id='',query='',url=TELLMEHUB_URL){
      if(query!='') query<-paste0('?',query)
      
      url<-paste0(url,'api/',object_type,'/',id,query)
      cat(paste0('obtaining json from url: ',url))
      export<-httr::GET(url = url)
      jsonunparsed<-httr::content(export,"text")
      #cat('...done')
      return(jsonunparsed)
    }
    
    # util method to invoke jq queries and return lazydt tables
    do_Q<-function(jj,q){
      #message("processing jq query")
      if(is.null(q)){ 
        warning("query is NULL")
        return(NULL)
      }
      jj %>% jqr::jq(as.character(q)) %>% 
        unique() %>% 
        j2dt()
    }
    
    # The method execute the x$q query against the jj$api json and returns the resulting data within a dptlyr::lazydt table.
    # Arguments:
    # jj: a named list containing json texts
    # x : a named list x with x$api = name to use for retrieving json object from the jj list
    #                           x$q   = jq query to execute against the json
    #                      and optionally
    #                           x$api_for_ids = name to identify the json object in jj to obtain a <list of ids> to invoke the url TELLMEHUB_URL/api/x$api/<list of ids> api
    #                           x$q_for_ids   = jq query to retrieve the ids to append to the x$api api endpoint
    #
    # When jj$api is not present:
    #   the query will be executed against the document the method will require to TELLMEHUB_URL/api/x$api endpoint.
    #   The optional arguments x$api_for_ids and x$q_for_ids must be given when the x$api needs a list of ids to be appended to the x$api request as argument.
    #   In that case the do_Qn method first retrieves the list of ids from the x$api_for_ids endpoint and executes the x$q_for_ids to obtain the ids,
    #   then it makes the api request and q query on the obtained document.
    do_Qn<-function(jj,x){
      if(!x$api %in% names(jj)){
        ids=""
        if("api_for_ids" %in% names(x)){
          ids<-getApiObjectsJson(object_type=x$api_for_ids) %>% do_Q(x$q_for_ids) %>% as_tibble() %>% pull() %>% paste0(collapse = ";")
        }
        jj[[x$api]]<-getApiObjectsJson(object_type = x$api, id = ids)
      }
      jj[[x$api]] %>% do_Q(x$q)
    }
    
    # prepare named list with json objects retrieved from TELLMEHUB_URL/api/...
    retrieveNamedListOfJsonObjects<-function(qn, read_online=TRUE){
      jj<-list()  # it's going to be pretty large... test it
      if(read_online){
        # prepare jj object with jsons.
        # if jj[[x$api]] exists, just returns it
        # otherwise the method makes a request to the TELLMEHUB_URL/api/x$api endpoint and returns the response.
        getJsonFromJJ<-function(jj,x){
          if(!x$api %in% names(jj)){
            ids=""
            if("api_for_ids" %in% names(x)){
              
              if(x[["api_for_ids"]] %in% names(jj))
                j4id<-jj[[x[["api_for_ids"]]]]
              else
                j4id<-getApiObjectsJson(object_type=x$api_for_ids)
              
              ids<-j4id %>% do_Q(x$q_for_ids) %>% as_tibble() %>% pull() %>% paste0(collapse = ";")
            }
            return(getApiObjectsJson(object_type = x$api, id = ids))
          }
          else
            return(jj[[x$api]])
        }
        #Omit the for loop in case of too large object. The rest of the code will be ok.
        for(n in names(qn)){
          jj[[qn[[n]]$api]]<-getJsonFromJJ(jj,qn[[n]])
        }
        return(jj)
      }
      else{
        # qn<-list(
        #   layers.ids=
        #     list(api="layers",     q=".objects[] | {layer_id: .id}"))
        warning("Creating offline TELLme object - reading main apis from cached json files")
        for(q in qn){ 
          jj[[q$api]]<-readLines(paste0("offline_files/jj.",
                                        gsub("/","_",q$api),".json"),-1)
        }
        return(jj)
      }
      
    }
    
    internal.getScrapingAuthSession<-function(usr=TELLME_HUB_USER, pwd=TELLME_HUB_PASSWORD){
      #if(grepl("account/login",res$url))
      message("- authenticating to tellmehub")
      require(rvest)
      login <- paste0(TELLMEHUB_URL,"account/login")
      #create a web session with the desired login address
      pgsession <- rvest::html_session(login)
      loginform <- pgsession %>% rvest::html_nodes("div#SigninModal form") %>% rvest::html_form()
      #pgform <- rvest::html_form(pgsession)
      filled_form <- rvest::set_values(loginform[[1]], username = usr, password = pwd)
      rvest::submit_form(pgsession, filled_form)
      return(pgsession)
    }
    
    getFeatureTypeGeometry<-function(featureTypeName,wfsUrl){
      if(wfsUrl=="") return(NA)
      require(xml2)
      res=""
      tryCatch(
        {
          url=sprintf("%s?service=WFS&version=1.1.0&request=DescribeFeatureType&typename=%s",wfsUrl,featureTypeName)
          message("trying url: ", url)
          xmldoc<-xml2::read_xml(url)
          # # check any issue (e.g. no record with the given name)
          # tryCatch({
          #   exc<-xmldoc %>% xml2::xml_find_all("//ows:Exception//text()")
          # },
          # error=function(e){warning(paste0("occhio!",e))},
          #   finally = function(){message("fatto")}
          # )
          geomtype<-xmldoc %>% xml2::xml_find_all("//xsd:extension[@base='gml:AbstractFeatureType']//xsd:element[contains(@type,'gml:')]/@type") %>% 
            xml2::xml_text()
          if(length(geomtype)==0)
            res="raster"
          else 
            res=geomtype[[1]]
        }, 
        error=function(e){
          warning("getFeatureTypeGeometry failed")
        },
        finally = {}
      )
      return(res)
    }
    
    info[["getGeometryTypesForLayerSet"]]<-"@param x: lazydt or tibble with variables 'layer_id', 'layer_storeType', 'layer_ows_url', 'layer_typename'\n
    @returns: tibble containing the same rows plus the field layer_geomType retrieved from ows service (or raster)"
    getGeometryTypesForLayerSet<-function(x){
      x %>% as_tibble() %>% 
        dplyr::rowwise(layer_id) %>% 
        dplyr::mutate(
          layer_geomType = if_else(layer_storeType=="coverageStore", 
                                   "raster", 
                                   if_else(layer_ows_url=="", 
                                           "", 
                                           getFeatureTypeGeometry(featureTypeName=layer_typename,wfsUrl=layer_ows_url) 
                                   )))
    }
    
    printHeader<-function(x, sep=", "){x %>% as_tibble()%>% colnames() %>% paste(collapse=sep)}
  }
  
  
  # initialize object ####
  init<-function(){
    tryCatch({
      
      message("* initializing tellme-hub object")
      message_inc<-(function(x=0){
        counter=x
        return(function(msg){
          counter<<-counter+1
          message(msg," ",counter)
        })
      })(0)
      
      message_inc("----- connecting to tellme-hub APIs ")
      
      jj<-retrieveNamedListOfJsonObjects(qn, read_online)
      
      
      ## if something changes in the qn object just execute again the following function
      ##  and copy-paste what it prints to regenerate the code which follows after function definition
      if(FALSE){
      #   (function() # code generation
      #   {
      #     cat("{  # autogenerated code \n")
      #     for (n in names(qn)){
      #       prefix=""
      #       if(!startsWith(n,"m2m_")) prefix="dt_"
      #       obj<-sprintf("%s%s",prefix,n)
      #       cat(sprintf("##--- %s ---##\n",obj))      #
      #       cat(sprintf("message(\"- populating %s ...\")\n",obj))
      #       cat(sprintf("%s <<- do_Qn(jj,qn$%s)\n",obj,n))
      #     cat(sprintf("message(printHeader(%s, sep=\" | \"), \"\\n\")\n\n",obj))
      #     }
      #     cat("}")
      #   })()
      }
      
      message_inc("----- creating tables")
      # CREATE TABLES with values 
      # (paste here under the autogenerated code with previous function)
      {  # autogenerated code 
        ##--- dt_layers.ids ---##
        message("- populating dt_layers.ids ...")
        dt_layers.ids <<- do_Qn(jj,qn$layers.ids)
        message(printHeader(dt_layers.ids, sep=" | "), "\n")
        
        ##--- dt_layers ---##
        message("- populating dt_layers ...")
        dt_layers <<- do_Qn(jj,qn$layers)
        message(printHeader(dt_layers, sep=" | "), "\n")
        
        ##--- m2m_layersKeywords ---##
        message("- populating m2m_layersKeywords ...")
        m2m_layersKeywords <<- do_Qn(jj,qn$m2m_layersKeywords)
        message(printHeader(m2m_layersKeywords, sep=" | "), "\n")
        
        ##--- dt_keywords ---##
        message("- populating dt_keywords ...")
        dt_keywords <<- do_Qn(jj,qn$keywords)
        message(printHeader(dt_keywords, sep=" | "), "\n")
        
        ##--- dt_maps.ids ---##
        message("- populating dt_maps.ids ...")
        dt_maps.ids <<- do_Qn(jj,qn$maps.ids)
        message(printHeader(dt_maps.ids, sep=" | "), "\n")
        
        ##--- m2m_mapsLayers ---##
        message("- populating m2m_mapsLayers ...")
        m2m_mapsLayers <<- do_Qn(jj,qn$m2m_mapsLayers)
        message(printHeader(m2m_mapsLayers, sep=" | "), "\n")
        
        ##--- dt_maps ---##
        message("- populating dt_maps ...")
        dt_maps <<- do_Qn(jj,qn$maps)
        message(printHeader(dt_maps, sep=" | "), "\n")
        
        ##--- m2m_mapsKeywords ---##
        message("- populating m2m_mapsKeywords ...")
        m2m_mapsKeywords <<- do_Qn(jj,qn$m2m_mapsKeywords)
        message(printHeader(m2m_mapsKeywords, sep=" | "), "\n")
        
        ##--- dt_styles ---##
        message("- populating dt_styles ...")
        dt_styles <<- do_Qn(jj,qn$styles)
        message(printHeader(dt_styles, sep=" | "), "\n")
        
        ##--- m2m_stylesLayers ---##
        message("- populating m2m_stylesLayers ...")
        m2m_stylesLayers <<- do_Qn(jj,qn$m2m_stylesLayers)
        message(printHeader(m2m_stylesLayers, sep=" | "), "\n")
        
      }
      
     
      message_inc("------ completing table information")
      # ALTER TABLES adding other infos to tables
      
      # MAPs table: add field "map_scale" (to each map attribute one (and only one) tellme scale)
      message("- adding 'map_scale' field to dt_maps")
      dt_maps<<-{dt_maps %>% 
        inner_join( ## construct a table that grants unicity of scale
          m2m_mapsKeywords %>% 
            inner_join(dt_keywords %>% 
              dplyr::filter(hkeyword_tellme_type=="scale") %>%  #tellme scales hk
              dplyr::select(hkeyword_id, map_scale=hkeyword_name)
            ) %>% 
            dplyr::select(map_id, map_scale) %>% 
            group_by(map_id) %>% #add_count(sort = T) %>% 
            mutate(map_scale=max(map_scale))%>%
            unique() 
        ) %>%
        dplyr::select(map_id, protocol_id, map_scale, 
                      category_identifier, map_uri, map_title, 
                      map_srid, map_projection, map_owner_username)
      }
      message(printHeader(dt_maps.ids, sep=" | "), "\n")
      
      #printHeader(dt_maps)
      #"map_id, protocol_id, map_scale, category_identifier, map_uri, map_title, map_srid, map_projection, map_owner_username"
      
      # # we need to add the "service_url" field to layers, then a new field "layer_geometry_type"
      # dt_layers %>% select(layer_storeType) %>% unique()
      # 
      # m2m_mapsLayers %>% select(layer_ows_url) %>% unique()
      
      ##ALTER TABLE LAYERS
      # add geometry type
      
      authsession<<-internal.getScrapingAuthSession()
      ###  - step 1 (ows is missing, geometry is missing)
      # scrape service page to
      # retrieve available remote services names and urls
      message_inc("----- scrape tellme hub to retrieve remote services")
      dt_services <<- {#internal.getScrapingAuthSession() %>% 
        authsession %>% 
        rvest::jump_to("/services") %>% 
        rvest::html_node(".table-striped") %>% 
        rvest::html_table() %>% 
        dplyr::select(layer_store=X1, layer_ows_url=X2)}
      
      message("- dt_services:")
      message(printHeader(dt_services, sep=" | "), "\n")
      
      
      #dt_layers %>% select(layer_storeType) %>% unique()
      # # set new layer_ows_url field
      # dt_layers2<-dt_layers %>% left_join(dt_services) %>% 
      #   mutate(layer_ows_url=if_else(is.na(layer_ows_url)==T, paste0(TELLMEHUB_URL,":80/geoserver/ows"), layer_ows_url)) %>% 
      #   as_tibble() %>% 
      #   dplyr::rowwise(layer_id) 
      
      message_inc("- adding 'layer_ows_url' field to dt_layers")
      ### - step 2: add and update the layer_ows_url column, and 
      #   - step 3: add and retrieve info about layer geometry type
      dt_layers <<- {
        dt_layers %>% left_join(dt_services) %>% 
        mutate(layer_ows_url=if_else(is.na(layer_ows_url)==T, paste0(TELLMEHUB_URL,"geoserver/ows"), layer_ows_url)) %>% 
        as_tibble() %>% 
        dtplyr::lazy_dt() # the double conversion seems necessary for dtplyr being able to further evaluate queries on dt_layers.
        # the used "if_else" is otherwise compromising other operations for upcoming raised error 
        # "Error in eval(expr, envir, enclos) : non trovo la funzione "fifelse"
        
        #%>% 
        # dplyr::rowwise(layer_id) %>% 
        # dplyr::mutate(
        #   layer_geomType = if_else(layer_storeType=="coverageStore", 
        #                            "raster", 
        #                            if_else(layer_ows_url=="", 
        #                                    "", 
        #                                    getFeatureTypeGeometry(featureTypeName=layer_typename,wfsUrl=layer_ows_url) 
        #                            )))
      }
      
      message_inc("- adding 'layer_geomType' field to dt_layers")
      #dt_layers <<- dt_layers %>% getGeometryTypesForLayerSet() %>% dtplyr::lazy_dt()
      
      
      
      message("dt_layers:")
      message(dt_layers %>% printHeader())
      
      # sublay<-dt_layers2 %>% filter(layer_id<100)
      # #TODO: far diventare una funzione in modo da limitare le chiamate solo quando si passa un fagiolone: in tal caso si valorizzi per i layer interessati.
      # loadLayersGeometry<-function(layers_subtable){
      #   
      #   layers_subtable %>% as_tibble() %>% 
      #     dplyr::rowwise(layer_id) %>% 
      #     dplyr::mutate(layer_geomType = if_else(layer_storeType=="coverageStore",
      #                              "raster",
      #                              if_else(layer_ows_url=="",
      #                                      "",
      #                                      !!getFeatureTypeGeometry(featureTypeName=layer_typename,wfsUrl=layer_ows_url)
      #                              )))
      # }
      # 
      # sublay %>% loadLayersGeometry()
      
      message("closing connections")
      closeAllConnections()
      
      # # retrieves all couples layer-concept (i.e. a layer will appear if it has a hkeyword with slug "concept_", and it will appear as many times as it present such kind of hkeywords)
      # # TODO: si può semplificare eliminando la necessità di mettere in join coi api.layers?
      # #       penso per esempio di aggiungere comunque nella api.layersKeywords anche i layer senza hk.
      # #       altrimenti per chiarezza andrebbe fatto tutto DAVVERO normalizzando.
      # #       Questo comporterebbe perà che le info transitorie che arrivano dalle "finte" m2m via API get-it
      # #       andrebbero inserite nella tabella dell'entità singola prima di essere eliminate.
      # #       Sarebbe senz'altro più pulito, ottenendo un modello molto più comprensibile.
      # allLayers2Concepts<<-(function(api.layers=api.layers, 
      #                                api.layersKeywords=api.layersKeywords, 
      #                                dt_Keywords=dt_Keywords){
      #   api.layers %>% 
      #     dplyr::left_join(api.layersKeywords) %>% 
      #     dplyr::inner_join((
      #       dt_Keywords %>% dplyr::filter(hkeyword_tellme_type=="concept")
      #     )) %>%
      #     dplyr::select(-layer_api_uri, -hkeyword_api_uri, -hkeyword_tellme_type) %>% 
      #     dplyr::mutate(concept_id=gsub("concept_","",hkeyword_slug)) %>% 
      #     dplyr::select(layer_id, hkeyword_id, concept_id, layer_name, layer_typename, layer_store, layer_storeType, hkeyword_name, hkeyword_slug) 
      # })()
      # 
      #####  MAPS ####
      # map details (keywords, i.e. scale_ and layers)
      
      # # MODEL map_id, map_scale, protocol_id
      # dt_Maps<<-(function(dt_Keywords, api.mapsKeywords){
      #   api.mapsKeywords %>% 
      #     dplyr::left_join(
      #       dt_Keywords %>% 
      #         dplyr::filter(hkeyword_tellme_type=="scale") %>% 
      #         dplyr::select(hkeyword_id, map_scale=hkeyword_name)
      #     )  %>% 
      #     as_tibble() %>% 
      #     tidyr::extract(col=category_identifier,into=c("protocol_id"),regex="protocol_(\\d+)" )%>% 
      #     dplyr::select(map_id, map_scale, protocol_id)
      # })(dt_Keywords, api.mapsKeywords)
      
      # protocol maps, ie. maps with a category with identifier= "protocol_"
      # map_id, map_scale, protocol_id  -- [removed: category_uri, category_description, map_uri, map_title, map_srid, map_projection, map_owner_username]
      
      #protocol_maps<-dt_maps %>% dplyr::filter(!is.na(protocol_id)) %>% dplyr::select(map_id, map_scale, protocol_id)

      # # all layers with at least one tellmeconcept as hk, and optionally some scale 
      # # (if the layer is within a protocol map and then it can be attributed the map (tellme) scale)
      # allLayers2Concepts %>% 
      #   left_join(m2m_mapsLayers %>% left_join(protocol_maps)) %>% 
      #   as_tibble() 
      
      # # layers with at least one concept, in at least one protocol.
      # layersForBeans<<-allLayers2Concepts %>% 
      #   inner_join(m2m_mapsLayers %>% inner_join(protocol_maps)) %>% 
      #   #as_tibble() %>% 
      #   dplyr::select(layer_id, layer_typename, layer_storeType, layer_local, layer_ows_url) %>% 
      #   unique() %>% 
      #   as_tibble() %>% 
      #   dplyr::rowwise(layer_id) %>% 
      #   dplyr::mutate(layer_geomType =
      #                   dplyr::case_when(
      #                     layer_storeType=="coverageStore" ~ "raster",
      #                     TRUE ~ as.character(getFeatureTypeGeometry(featureTypeName=layer_typename,wfsUrl=layer_ows_url))
      #                   )
      #   )
      # # DONE: add to each of these layers its geometryType
      # 
      if(writeJJtoDisk){
        message_inc("**** WRITING JSON OBJECTS TO DISK ******")
        #jj %>% write(file="jj.rdata")
        for(api in names(jj)){
          #api<-names(jj)[1]
          apisub<-(gsub("/","_",api))
          file=file(paste0("offline_files/jj.",apisub,".json"))
          jj[[api]] %>% writeChar(con=file)
          close(file)
        }
      }
      rm(jj)
      
      
    },
    error=function(e){
      warning(e)
    },
    finally=function(){
      message("TELLme-Hub object initialized")
      # if(writeJJtoDisk){
      #   message("**** WRITING JSON OBJECTS TO DISK ******")
      #   #jj %>% write(file="jj.rdata")
      #   for(api in names(jj)){
      #     #api<-names(jj)[1]
      #     file=file(paste0("jj.",api,".json"))
      #     jj[[api]] %>% writeChar(con=file)
      #     close(file)
      #   }
      # }
      # rm(jj)
    })
  }

  init()
  
  # EXPORT "OBJECT"
  ####### code generator (qn must be set) #####
  ## produce statements for exported variabiles, the code must be put at the beginning of the next section
  #
  # (function(){
  #   cat("{ # auto generated code\n")
  #   for (n in names(qn)){
  #     prefix=""
  #     if(!startsWith(n,"m2m_")) prefix="dt_"
  #     obj<-paste0(prefix,n)
  #     cat(sprintf("self$%s<-function(){%s}\n",obj, obj))
  #   }
  #   cat("} # --- \n")
  # })()
  ###
  
  
  { # auto-generated code
    self$dt_layers.ids<-function(){dt_layers.ids}
    self$dt_layers<-function(){dt_layers}
    self$m2m_layersKeywords<-function(){m2m_layersKeywords}
    self$dt_keywords<-function(){dt_keywords}
    self$dt_maps.ids<-function(){dt_maps.ids}
    self$m2m_mapsLayers<-function(){m2m_mapsLayers}
    self$dt_maps<-function(){dt_maps}
    self$m2m_mapsKeywords<-function(){m2m_mapsKeywords}
    self$dt_styles<-function(){dt_styles}
    self$m2m_stylesLayers<-function(){m2m_stylesLayers}
  } # --- 
  self$dt_services<-function(){dt_services}
  
  # methods to explore layers with and without tellme related concept (hkeywords)
  # TODO: CHECK THIS
  select_layersWithConcepts<-function(tellme_type="concept"){
    message("To see the concepts please left join with m2m_layersKeywords and dt_keywords")
    m2m_layersKeywords %>% 
      inner_join(dt_keywords %>% 
                   dplyr::filter(hkeyword_tellme_type==!!tellme_type)) %>% 
      dplyr::select(hkeyword_id, layer_id, hkeyword_tellme_type)  %>%
      dplyr::select(layer_id) %>% unique() %>% inner_join(dt_layers)
  }
  if(exportUtilsFunctions)
    utils$select_layersWithConcepts <- select_layersWithConcepts

  if(exportUtilsFunctions){
    utils$select_layersWithoutConcepts<-function(){
      `%notin%` <<- Negate(`%in%`)
      dt_layers %>% dplyr::anti_join(select_layersWithConcepts())
    }
  }
  
  
  select_mapsAtScale<-function(scale="XL"){
    dt_maps %>% dplyr::filter(map_scale==!!scale)
  }
  if(exportUtilsFunctions)
    utils$select_mapsAtScale<-select_mapsAtScale()
  
  select_layerIdsAtScale<-function(scale="XL"){
    m2m_mapsLayers %>% inner_join(select_mapsAtScale(scale)) %>% inner_join(dt_layers) %>% dplyr::select(layer_id) %>% unique()
  }
  if(exportUtilsFunctions)
    utils$select_layerIdsAtScale<-select_layerIdsAtScale
  
  select_m2m_layersConcepts<-function(){
    dt_keywords %>% dplyr::filter(hkeyword_tellme_type=="concept") %>% dplyr::select(hkeyword_id, hkeyword_slug) %>% inner_join(m2m_layersKeywords)
  }
  if(exportUtilsFunctions)
    utils$select_m2m_layersConcepts<-select_m2m_layersConcepts
  
  # table containing concept_ids
  select_layersBy.tibbleOf.concept_id<-function(t, scale="L"){
    # note: from glossary we expect a bean obtained like this:
    #   t<-glossary$beanWithPerspectivesByDynamicId_tibble(21)
    # note2: to retrieve the scale, given the dynamic_id, execute:
    #   scale<-glossary$mm2mm_DynamicsSemanticPackagesIssues()%>% dplyr::filter(dynamic_id==21) %>% dplyr::select(package_scale) %>% pull()
    #dt_keywords<-hub$dt_keywords()
    #m2m_layersKeywords<-hub$m2m_layersKeywords()
    #dt_layers<-hub$dt_layers()
    # add hkeyword_slug column to naturally join with m2m_layersConcepts
    conceptList<-t %>% dplyr::mutate(hkeyword_slug=paste0("concept_",concept_id)) %>% dtplyr::lazy_dt() # lista concetti da cercare
    #conceptList
    conceptList %>% 
      left_join(select_m2m_layersConcepts() %>% inner_join(select_layerIdsAtScale(scale))) %>% 
      left_join(dt_layers) %>% dplyr::select(keyword_id, keyword_title, concept_id, concept_title, everything(), -hkeyword_slug, -hkeyword_id) %>% # così vedo almeno una riga per ogni concetto nel bean
      as_tibble() 
    
    #select_m2m_layersConcepts() %>% inner_join(conceptList) %>% inner_join(dt_layers) #così vedrei solo righe di concetti del bean con almeno un layer
    #select_layersAtScale()
  }
  if(exportUtilsFunctions)
    utils$select_layersBy.tibbleOf.concept_id<-select_layersBy.tibbleOf.concept_id
  
  # NOTE: it does output only concepts with layers. Missing layers are not output.
  info[["layersInBean"]]<-"Function that given a Bean and a (tellme)scale [e.g. \"XL\" or \"L\"] \n
  Bean: a tibble with variable \"concept_id\" containing values like \"concept_12\" \n
  returns a tibble with the available layers for the indicated tellme concepts.\n
  NOTE: concepts with no associate layer will not be returned (to have all concepts with or witOUT layers please use the function beanWithLayers"
  self$layersInBean<-function(bean, scale){
    message("to retrieve the complete bean even with concepts without layers, use beanWithLayers function")
    #bean<-t
    #scale="L"
    select_layersBy.tibbleOf.concept_id(bean,scale) %>% 
      dplyr::filter(!is.na(layer_id)) %>% 
      dplyr::select(-layer_api_uri, -layer_name, -layer_title, -layer_alternate) %>% 
      getGeometryTypesForLayerSet()
    
    #hub$layersInBean(glossary$beanWithPerspectivesByDynamicId_tibble(21),scale="M") 
    #   %>% hub$getGeometryTypesForLayerSet()
  }
  
  info[["beanWithlayers"]]<-"INPUT: a Bean and a (tellme)scale [e.g. \"XL\" or \"L\"] \n
  \nBean: a tibble with variable \"concept_id\" containing values like \"concept_12\" \n
  RETURNS: a tibble with the available layers for the indicated tellme concepts.\n
  NOTE: concepts with no associate layer WILL BE returned (to retrieve only the concepts WITH layers please use the function layersInBean"
  self$beanWithlayers <- function(bean, scale){
    select_layersBy.tibbleOf.concept_id(bean,scale) %>% 
      dplyr::select(-layer_api_uri, -layer_name, -layer_title, -layer_alternate)
  }
  
  self$getGeometryTypesForLayerSet<-getGeometryTypesForLayerSet
  
  # # method to explore layers without tellme related concept hkeywords 
  # self$getLayersWithoutAssociatedTELLmeConcept<-function(){
  #   `%notin%` <<- Negate(`%in%`)
  #   m2m_layersKeywords %>% dplyr::filter(layer_id %notin% (
  #     allLayers2Concepts %>% dplyr::select(layer_id) %>% unique() %>% pull(layer_id))
  #   ) %>% dplyr::left_join(dt_Keywords, on=c("hkeyword_id"="hkeyword_id")) %>% as_tibble() %>% 
  #     dplyr::select(layer_id, hkeyword_id, layer_typename, hkeyword_slug, hkeyword_name, tidyr::everything())
  # }
  #self$layersForBeans<-function(){layersForBeans}
  
  self$getFeatureTypeGeometry<-getFeatureTypeGeometry
  
  self$summary<-function(){
    table_with_counts_message<-function(tablename){
      #cat(
        sprintf("Table %s \t type:%s, \t with \t%d records", tablename, class(self[[tablename]]())[1], self[[tablename]]() %>% count() %>% pull())
      #)
    }
    cat("tellme hub object, containing the following tables:\n\n")
    
    for(t in names(self)){
      if(t %>% startsWith("dt_") || t %>% startsWith("m2m_"))
        cat("\n",table_with_counts_message(t))
      else {
        cat(paste("\n",t, "\tinfo:", info[[t]]))
      }
    }
    
  }
  
  # ## TODO (?): methods that write information into the TELLme Hub
  # # associate a hkeyword to the specified layer (then reload the layers)
  # setLayerKeyword<-function(l_id, hkeyw_id){
  #   #TO BE DONE
  # }
  # 
  # # associate a layer to a map (then reload the maps)
  # setLayerMap<-function(l_id,map_id){
  #   #TO BE DONE
  # }
  # 
  # # associate a category to a map (then reload the maps)
  # setMapCategory<-function(map_id,category_id){
  #   #TO BE DONE
  # }
  if(exportUtilsFunctions) {
    #utils<-list()
    utils$getApiObjectsJson<-getApiObjectsJson
    #.... add here other utils to be exported on demand
    self$utils<-utils
  }
  
  if(exportAdminFunctions){
    admin<-list()
    
    admin.GET<-function(url){
      res<-httr::GET(url = url,handle = authsession$handle)
      if(grepl("account/login", res$url)){
        authsession<<-internal.getScrapingAuthSession()
        warning("authenticated session was expired. Retry.")
      }
      return(res)
    }
    
    #sprintf("",)
    admin$mapid_set_scale<-function(map_id,scale){
      s=sprintf(paste0(TELLMEHUB_URL,"tellme/maps/%d/set_scale/%s"), map_id, scale)
      res<-admin.GET(s)
      # res$content
      return(res)
    }
    admin$mapid_set_protocol<-function(map_id,protocol_id){
      s=sprintf(paste0(TELLMEHUB_URL,"tellme/maps/%d/set_protocolid/%d"), map_id, protocol_id)
      res<-admin.GET(s)
      # if(!is.null(content(res)$success) && content(res)$success) {
      #   dt_map %>% mutate(category_identifier=if_else())
      # }
      return(res)
    }
    admin$layername_set_conceptid<-function(layername,concept_id, add=FALSE){
      s=sprintf(paste0(TELLMEHUB_URL,"tellme/layers/%s/set_conceptid/%d"), layername,concept_id)
      if(add) s=paste0(s,"?add=TRUE")
      res<-admin.GET(s)
      # cr<-httr::content(res)
      # if(!is.null(cr$success) && cr$success){
      #   if(!add){
      #     m2m_layer_keywords<<-m2m_layer_keywords %>% filter(layer_id!=!!cr$layer_id)
      #   }
      #   m2m_layer_keywords<<-m2m_layer_keywords %>% add_row(layer_id=!!cr$layer_id,hkeyword_id=!!cr$hkeyword_id)
      # }
      return(res)
    }
    admin$layerid_set_conceptid<-function(layer_id,concept_id, add=FALSE){
      s=sprintf(paste0(TELLMEHUB_URL,"tellme/set_layerid_conceptid/%d/%d"), layer_id, concept_id)
      if(add)s=paste0(s,"?add=TRUE")
      res<-admin.GET(s)
      # cr<-httr::content(res)
      # if(!is.null(cr$success) && cr$success){
      #   if(!add){
      #     m2m_layer_keywords<<-m2m_layer_keywords %>% filter(layer_id!=!!cr$layer_id)
      #   }
      #   m2m_layer_keywords<<-m2m_layer_keywords %>% add_row(layer_id=!!cr$layer_id,hkeyword_id=!!cr$hkeyword_id)
      # }
      return(res)
    }
    
    
    if(F){
    # admin.GET<-function(url, getit_superuser, getit_password){
    #   
    #   urlauth<-paste0(TELLMEHUB_URL,"account/login/")
    #   res<-httr::GET(url=urlauth) # questo dovrebbe servire a settare i cookie necessari alla POST successiva
    #   
    #   # obtain csfrtoken
    #   s<-res$headers$`set-cookie` # e.g. "csrftoken=saknwlpo8G64UKJDR7UGxNbue184D9if; expires=Mon, 08-Feb-2021 15:12:47 GMT; Max-Age=31449600; Path=/"
    #   s_split<-strsplit(s,";")
    #   s_kv<-grep("csrftoken",fixed=TRUE,s_split[[1]],value=TRUE)
    #   csfrtoken<-sub("csrftoken=","",s_kv)
    #   
    #   reslogin<-httr::POST(url=urlauth,
    #                        body=list(csrfmiddlewaretoken=csfrtoken,
    #                                  username=getit_superuser,
    #                                  password=getit_password)
    #   )
    #   
    #   res<-httr::GET(url = url,
    #                  query=query
    #   )
    #   return(res)
    # }
    }
    # layertitle: field title of both get-it and geoserver. It will be keeped as passed (e.g.uppercase)
    # on the contrary, the layer name is for example lowercased version of the Title
    admin$getit_uploadLayer<-function(zipname_and_path, layertitle=NULL, the_abstract="", getit_user=TELLME_HUB_USER, getit_userpassword=TELLME_HUB_PASSWORD){
        getit_url<-TELLMEHUB_URL
        
        zipname<-basename(zipname_and_path)
        #layername<-layertitle
        geoserver_layername=sub(".zip","",zipname,fixed = T)
        if(is.null(layertitle)){
          layertitle=geoserver_layername
          #TODO: verificare nome layer in geoserver. Potrebbe 1. alterare alcuni caratteri; 2. non coincidere con layertitle passato al get-it
          # in questo caso bisogna agire di conseguenza sulla parte di codice che assegna la keyword e che poi esegue updatelayers
        }
        
        ### auth
        {
          urlauth<-paste0(getit_url,"account/login/")
          res<-httr::GET(url=urlauth) 
          
          # obtain csfrtoken
          s<-res$headers$`set-cookie` # e.g. "csrftoken=saknwlpo8G64UKJDR7UGxNbue184D9if; expires=Mon, 08-Feb-2021 15:12:47 GMT; Max-Age=31449600; Path=/"
          s_split<-strsplit(s,";")
          s_kv<-grep("csrftoken",fixed=TRUE,s_split[[1]],value=TRUE)
          csfrtoken<-sub("csrftoken=","",s_kv)
          
          reslogin<-httr::POST(url=urlauth,
                               body=list(csrfmiddlewaretoken=csfrtoken,
                                         username=getit_user,
                                         password=getit_userpassword)
          )
        }
        
        ccc<-cookies(reslogin)
        loggedin_token<-ccc$value[ccc$name=="csrftoken"]
        loggedin_session<-ccc$value[ccc$name=="sessionid"]
        response<-httr::POST(paste0(getit_url,"layers/upload"),
                             config = httr::add_headers(`X-CSRFToken`=loggedin_token, Accept="*/*", 
                                                        Origin=getit_url, 
                                                        Connection="keep-alive", 
                                                        Referer=paste0(getit_url,"layers/upload")
                             ),
                             body=list(base_file=upload_file(zipname_and_path),
                                       permissions='{"users":{"AnonymousUser":["view_resourcebase","download_resourcebase"]},"groups":{}}',
                                       zip_file=upload_file(zipname_and_path),
                                       charset="UTF-8",
                                       abstract=the_abstract,
                                       layer_title=layertitle)
        )
        # extract geoserver_layername from response if possible
        getit_layerurl<-content(response)$url
        getit_layername<-sub("/layers/","",getit_layerurl)
        geoserver_layertitle<-layertitle # it is unchanged with respect to the layertitle passed through the POST request
        geoserver_layername<-getit_layername
        # TODO: check return values for names of the layers in geoserver and in get-it: are they correct? are they the same?
        return(list(status=status_code(response),getit_layername=getit_layername, geoserver_layername=geoserver_layername, getit_layerurl=getit_layerurl, response=response))
    }
    # output<-hub$admin$getit_uploadLayer(...)
    # then use output$getit_layername as the layer name for further operations like setting the concept_id
    
    
    self$admin<-admin
  }
  
  self$is_reading_online<-function(){return(read_online)}
  self$set_read_online<-function(online){read_online<<-online}
  self$refresh<-function(online=read_online){
    if(online!=read_online)
      read_online<<-online
    init()
  }
  
  return(self)
}
# set to TRUE in order to load an instance of hub while sourcing
if(TRUE){
  hub<-getTELLmeHub(
    read_online = T,
    TELLMEHUB_URL=TELLMEHUB_URL, 
    TELLME_HUB_USER = TELLME_HUB_USER, 
    TELLME_HUB_PASSWORD = TELLME_HUB_PASSWORD, 
    exportUtilsFunctions = TRUE,
    writeJJtoDisk = FALSE,
    exportAdminFunctions = TRUE)
}
if(FALSE){ #EXAMPLE USAGE of $admin functions
  # the_concept_id=77
  # output_newLayer<-hub$admin$getit_uploadLayer(zipname_and_path = "prova.zip",layertitle = "prova")
  # #status
  # #getit_layername
  # #geoserver_layername
  # #getit_layerurl
  # #response
  # if(output_newLayer$status==200){
  #   lname<-output_newLayer$getit_layername
  #   hub$admin$layername_set_conceptid(lname, the_concept_id)
  # }
  
  # res<-hub$admin$mapid_set_protocol(485,1)
  # (res %>% content())$success
  # 
  # #http://tellmehub.get-it.it/tellme/layers/geonode:areas_geoestadisticas_municipales/set_conceptid/77
  res<-hub$admin$layername_set_conceptid(layername = "geonode:areas_geoestadisticas_municipales", concept_id = 77,add=FALSE)
  # res %>% content()
  uploadedLayerOutput<-hub$admin$getit_uploadLayer(zipname_and_path = "./_trash/layerprova/PROVAPROVA/Archivio.zip",layertitle = "PROVAPROVA",the_abstract = "UPLOAD_TEST")
  
  if(uploadedLayerOutput$status==200)
    print("success")
  else
    print("failure")
    
  
  getit_updatelayers(getit_url = "http://tellmehub.get-it.it", getit_superuser = "admin", getit_password = "admin", workspacename = "geonode", datastorename = "geonode_data",layername = "provaprova", ownername = "admin")
  
  #hub$admin$getit_uploadLayer()
  # TODO: when upload is done correctly reload the hub object
  #         hub$refresh(online = TRUE)
  #       ** or add lines in internal datatables **
  # TODO: upload new style: 
  #   http://tellmehub.get-it.it/layers/geonode:gua_ran_nucleosagrarios_1/style_upload (?)
  # Follow the http workflow and implement it (rvest?)
  # TODO: manage styles: add existing style to layer, set layer default style, 
  #   /gs/geonode:gua_ran_nucleosagrarios_1/style/manage (POST FORM)
  # Same as previous, or consider using geoserver directly (cf. utils.R) after evaluating behaviour of geonode sw layer.
}

