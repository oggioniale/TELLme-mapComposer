# require(jqr)
# require(httr)
# require(dplyr)
# require(jsonlite)
# require(openssl)
# #install.packages('dtplyr')
# require(dtplyr)
# 
# # packages for web scraping
# require(rvest)
# require(stringr)
# require(purrr)
#require(tidyr)

source("accounts_private.R")

# imports:
require(jqr);require(httr);require(jsonlite)
require(openssl);require(rvest);require(stringr)
require(dplyr);require(dtplyr);require(purrr);require(tidyr)

getGlossary <- function(read_online=TRUE) {
  # imports:
  require(jqr);require(httr);require(jsonlite)
  require(openssl);require(rvest);require(stringr)
  require(dplyr);require(dtplyr);require(purrr);require(tidyr)
  
  # funtional object factory
  # self is the final return value: it is an "instance" exposing internals.
  # To refresh the object instance use the exposed "refresh" method.
  self = list()
  
  internal.getGlossarySoftwareJsonDump <-
    function(url = TELLME_GLOSSARY_URL,
             user = TELLME_GLOSSARY_USER,
             password = TELLME_GLOSSARY_PASSWORD) {
      message('obtaining latest json dump from polimi tellme glossary software...')
      token <-
        openssl::base64_encode(charToRaw(paste(user, password, sep = ":")))
      if(read_online){
        export <-
          httr::GET(url = url, add_headers("Authorization" = paste("Basic", token)))
      }
      else export=list(status_code=-1)
      
      if (export$status_code != 200) {
        jsonunparsed <- readLines("offline_files/export_glossary.json",-1)
        if(read_online) warningMsg<-paste0(
          "Unable to read latest glossary version: polimi software returned status code ",
          export$status_code, '. The last cached version will be used')
        else warningMsg<-"You called getGlossary(online=FALSE): retrieving glossary offline from the last cached version"
        warning(warningMsg)
      }
      else{
        #export$content
        jsonunparsed <- httr::content(export, "text")
        writeLines(jsonunparsed,"offline_files/export_glossary.json")
      }
      message('...done')
      return(jsonunparsed)
    }
  
  ## internal variables
  {
    #jj<-NULL 
    # data tables
    dt_Protocols<-NULL
    dt_Issues<-NULL
    dt_SemanticPackages<-NULL
    dt_Dynamics<-NULL
    dt_Concepts<-NULL
    dt_Keywords<-NULL
    m2m_ProtocolScaleConcepts<-NULL
    m2m_DynamicsConcepts<-NULL
    m2m_IssuesSemanticPackages<-NULL
    mm2mm_DynamicsSemanticPackagesIssues<-NULL
    
    dynamicBeanWithPerspectives_tibble<-list()
  }# internal variables
  
  # jq queries
  {
    q<-list(
      protocols='.protocols[] | {
            protocol_id:.id, protocol_title:.title, entryType:.entryType,
            scale: .scales[] | .scale}',
      issues='.issues[] | {
              issue_id:.id, issue_title:.title, entryType:.entryType, 
              comment:.comment,context:.context,meaning:.meaning,reference:.reference,
              operator_ids:(.operatorIds | tostring)}',
      semanticPackages='.semanticPackages[] | {
      package_id:.id, scale:.scale, issue_id:.issueId, metropolis:.metropolis.title}',
      dynamics='.dynamics[] | {
          dynamic_id:.id, dynamic_title:.title, package_id:.semanticPackageId, conceptCount:(.aggregate.concepts | length)
      }',
      concepts='.concepts[] | {
          concept_id:.id, concept_title:.title, 
          scalesAsText:.scalesAsText,
          entryType:.entryType,
          keyword_id:.keywordId
      }',
      keywords='.keywords[] | {
          keyword_id:.id, keyword_title:.title, meaning:.meaning, reference:.reference,
          entryType:.entryType
      }',
      m2m_ProtocolScaleConcepts='.protocols[] | {
            protocolId:.id, 
            scale: [(
              .scales[] | {scale: .scale, conceptId:.concepts[].id}
            )]
          } |
          {protocolId:.protocolId, scale_concept:(.scale[] | [.scale, .conceptId])} |
          {protocol_id:.protocolId, scale:.scale_concept[0], concept_id:.scale_concept[1]}',
      m2m_DynamicsConcepts='.dynamics[] |{dynamic_id:.id, concept_id:.aggregate.concepts[].id}'
    )
  }# jq queries
  
  # util method to invoke jq queries and return lazydt tables
  do_Q<-function(q,jj){
    jj %>% jqr::jq(as.character(q)) %>% 
      textConnection() %>% jsonlite::stream_in(simplifyDataFrame=TRUE) %>% 
      dtplyr::lazy_dt()
  }
  
  #### scrape web pages for entities not present in the polimi tellme glossary software json dump
  # (connects to server)
  internal.getScrapingAuthSession<-function(usr=TELLME_GLOSSARY_USER, pwd=TELLME_GLOSSARY_PASSWORD){
    require(rvest)
    login <- "http://www.tellme.polimi.it/tellme_apps/tellme/login"
    #create a web session with the desired login address
    pgsession <- rvest::html_session(login)
    pgform <- rvest::html_form(pgsession)
    filled_form <- rvest::set_values(pgform[[1]], username = usr, password = pwd)
    rvest::submit_form(pgsession, filled_form)
    return(pgsession)
  }
  
  # (connects to server)
  fagioloneWebByURL<-function(url, usr=TELLME_GLOSSARY_USER, pwd=TELLME_GLOSSARY_PASSWORD){
    message(paste("retrieving online version from tellme glossary software at url...",url))
    require(rvest)
    require(stringr)
    require(dplyr)
    require(purrr)
    require(dtplyr)
    
    pgsession<-internal.getScrapingAuthSession()
    page<-rvest::jump_to(pgsession, url)
    #concetti<-rvest::html_nodes(page, "div .concept")
    
    bean<-rvest::html_nodes(page, "div .concept") %>% as.list() %>% 
      purrr::map_dfr(function(x){
        return(data.frame(id=rvest::html_attr(x,"data-conceptid"),
                          title=rvest::html_text(x) ))
      })%>% as_tibble() %>% dtplyr::lazy_dt()
    
    # extract id of selected concepts from javascript code in the page
    scripts<-rvest::html_nodes(page, "script:contains('concept-selected')") # the script is the one containing this string
    if(length(scripts)<1){
      warning("...no script with concept selection found (maybe a protocol?)")
      return(bean)
    }
    s<-rvest::html_text(scripts[[1]])
    
    ids_selected<-stringr::str_match_all(s,stringr::regex("data-conceptid='(\\d+)'", multiline=TRUE))[[1]][,2] %>% 
      dplyr::as_tibble() %>% dtplyr::lazy_dt() %>% 
      rename(id=value) %>% dplyr::mutate(selected=1, concept_slug=paste0("concept_",id))
    
    message("...done")
    
    bean %>% left_join(ids_selected) 
  }
  # # example usage:
  # dyn21url="http://www.tellme.polimi.it/tellme_apps/tellme/dynamic/21"
  # fagioloneWebByURL(dyn21url)
  
  # dynamic specific function
  # (connects to server)
  beanFromDynamicId<-function(id){
    url=paste0("http://www.tellme.polimi.it/tellme_apps/tellme/dynamic/",id)
    fagioloneWebByURL(url)
  }
  # #example usage:
  # beanFromDynamicId(21)
  
  # perspective specific function
  # (connects to server)
  beanFromPerspectiveId<-function(id){
    url=paste0("http://www.tellme.polimi.it/tellme_apps/tellme/perspective/",id)
    fagioloneWebByURL(url)
  }
  
  # beanFromPerspectiveId(26)
  
  # fagioloneWebByURL("http://www.tellme.polimi.it/tellme_apps/tellme/protocol/43")
  
  # extract for a given package the (one2many) table of dynamic_id <- perspective_id.
  # (connects to server)
  perspectivesByPackageId<-function(package_id){
    message("retrieving online version from tellme glossary software.")
    pgsession<-internal.getScrapingAuthSession()
    u<-paste0("http://www.tellme.polimi.it/tellme_apps/tellme/package/",package_id)
    page<-rvest::jump_to(pgsession, u)
    html_nodes(page,"div#accordion > .card") %>% 
      purrr::map_dfr(function(x){
        #x<-html_nodes(page,"div#accordion > .card")[2]
        dynamic<- x %>% html_nodes("button span") %>% html_text()
        dynamic_url<-x %>% html_nodes("a:contains('View dynamic')") %>% html_attr("href")
        dyn_id<-as.integer(stringr::str_match(dynamic_url,stringr::regex("dynamic/(\\d+)"))[1,2])
        
        dyn_table<-list(dynamic_id=dyn_id,dynamic_title=dynamic) %>% as_tibble()
        persp_table <- x %>% html_nodes(".card-body a") %>% 
          purrr::map_df(function(y){
            
            y %>% html_attr("href") %>% as_tibble() %>%
              rename(perspective_url=value) %>% 
              mutate(dynamic_id=dyn_id,
                     perspective_id=as.integer(str_match(perspective_url,stringr::regex("perspective/(\\d+)"))[1,2]),
                     perspective_title=(y %>% html_text()))
          }) 
        
        if(length(persp_table)==0){
          persp_table=as_tibble(list(perspective_url=NA,dynamic_id=dyn_id, perspective_id=as.integer(NA), perspective_title=as.character(NA)))
        }
        
        full_join(dyn_table, persp_table) %>% dplyr::select(-perspective_url) 
        #return(list(d=dyn_table,p=persp_table))
      }) 
  }# returns a tibble
  
  # (connects to server)
  perspectivesByDynamicId<-function(dyn_id){
    # NOTE: the result of a table with 0 rows, means that the dynamic has no perspective: one can know it in advance
    # by inspecting the "dynamic_conceptCount" column of the mm2mm_DynamicsSemanticPackagesIssues table
    packageID<- dt_Dynamics %>% 
      dplyr::filter(dynamic_id==!!dyn_id) %>% 
      dplyr::select(package_id) %>% 
      as_tibble() %>% as.integer()
    perspectivesByPackageId(packageID) %>% 
      #dplyr::filter( !is.na(perspective_id)) %>% 
      dplyr::filter(dynamic_id==!!dyn_id) 
  }
  # e.g.
  #perspectivesByPackageId(21)
  
  # (connects to server if bean is not loaded yet or if refresh=TRUE)
  beanWithPerspectivesByDynamicId_tibble<-function(dyn_id, refresh=FALSE){
    require(tidyr);require(dplyr);require(purrr)
    sdyn_id<-dyn_id %>% as.character()
    
    #browser()
    if(refresh || is.null(dynamicBeanWithPerspectives_tibble[[sdyn_id]])){
      
      idsVect<-perspectivesByDynamicId(dyn_id) %>% dplyr::select(perspective_id) %>% as.vector() %>% unlist() %>% as.numeric() %>% as.character()
      dynamic_has_perspective<-!is.na(idsVect[1])
      
      if(dynamic_has_perspective){
        # dyn_id<-21;sdyn_id="21"
        message(paste0("...lazy loading bean for dynamic ",sdyn_id))
        
        # retrieve character vector of perspective ids
        #idsVect<-perspectivesByDynamicId(dyn_id) %>% 
        #  dplyr::select(perspective_id) %>% as.vector() %>% unlist() %>% as.numeric() %>% as.character()
        # obtain all the the perspective beans and join them naming "selectedByPerspective.X" the column from bean X
        listOfPerspectiveBeans=list()
        for(i in idsVect){
          #  cat(print(paste0("\n:",i)))
          selColname=paste0("selectedByPerspective.",i)
          listOfPerspectiveBeans[[i]]<-beanFromPerspectiveId(i) %>% as_tibble() %>% 
            tidyr::replace_na(list(selected=0)) %>% 
            dplyr::select(concept_id=id, concept_title=title, !!selColname:=selected) 
          #tibble  glossary$beanFromPerspectiveId(i)
        }
        # create one new column (corresponding to the whole "dynamic" selected column) counting how many times the 
        # concept is selected (i.e. summing the "1"s in the "selectedByPerspective.X" columns)
        
        dynamicBeanWithPerspectives_tibble[[sdyn_id]]<<-
          listOfPerspectiveBeans %>% 
          purrr::reduce(dplyr::left_join, by=c("concept_id", "concept_title")) %>% 
          dplyr::group_by(concept_id, concept_title) %>% 
          tidyr::nest() %>% # ora sommo ogni elemento in "data":
          dplyr::mutate(is_selected=purrr::map_dbl(data,sum)) %>% 
          tidyr::unnest(cols = c(data)) %>% # TODO: use cols = c(data) (`cols` is now required when using unnest().)
          dplyr::ungroup() %>% 
          dplyr::mutate(concept_id=as.integer(concept_id)) %>% 
          inner_join(dt_Concepts %>% dplyr::select( -concept_scalesAsText) %>% as_tibble() )
        
        
        rm(listOfPerspectiveBeans)
      }
      else{
        warning("dynamic has no perspective")
          # dyn_id<-7;sdyn_id="7"
          #concept_id (int), concept_title, is_selected, keyword_id (int), keyword_title
          dynamicBeanWithPerspectives_tibble[[sdyn_id]]<<- beanFromDynamicId(dyn_id) %>% as_tibble() %>% 
            dplyr::mutate(concept_id=as.integer(id), is_selected=0) %>% 
            dplyr::select(concept_id, concept_title=title, is_selected) %>% 
            inner_join(dt_Concepts %>% dplyr::select(-concept_scalesAsText) %>% as_tibble() ) %>% 
            dplyr::select(concept_id, concept_title, is_selected, keyword_id, keyword_title) %>% as_tibble()
      }
    }
    
    return(dynamicBeanWithPerspectives_tibble[[sdyn_id]])
  }
  
  #######
  # initialize object
  init<-function(){
    jj <- internal.getGlossarySoftwareJsonDump()
    
    message("...updating glossary object tables...")
    dt_Protocols <<- do_Q(q$protocols,jj)
    dt_Issues<<-do_Q(q$issues,jj)
    dt_SemanticPackages<<-do_Q(q$semanticPackages,jj)
    dt_Dynamics<<-do_Q(q$dynamics,jj)
    dt_Keywords<<-do_Q(q$keywords,jj)
    dt_Concepts<<-do_Q(q$concepts,jj) %>% 
      dplyr::rename(concept_scalesAsText=scalesAsText) %>% 
      dplyr::select(-entryType) %>% 
      dplyr::inner_join(dplyr::select(.data=dt_Keywords, -entryType, -meaning, -reference)) %>% 
      dplyr::select(concept_id, everything())
    m2m_ProtocolScaleConcepts<<-do_Q(q$m2m_ProtocolScaleConcepts,jj)
    m2m_DynamicsConcepts<<-do_Q(q$m2m_DynamicsConcepts,jj)
    
    m2m_IssuesSemanticPackages<<-dt_Issues %>% dplyr::right_join(dt_SemanticPackages) %>% dplyr::select(issue_id, issue_title, package_id, scale, metropolis)
    
    mm2mm_DynamicsSemanticPackagesIssues<<-dt_Dynamics %>% 
      dplyr::left_join(dt_SemanticPackages) %>% 
      #dplyr::filter(conceptCount>0) %>% 
      dplyr::left_join(dt_Issues %>% dplyr::select(issue_id, issue_title) ) %>% 
      dplyr::mutate(packageTitle=paste(issue_title,scale)) %>% 
      #rename(package_scale=scale, package_metropolis=metropolis, dynamic_conceptCount=conceptCount) %>% 
      dplyr::select(dynamic_id, dynamic_title, dynamic_conceptCount=conceptCount, package_id, packageTitle, package_scale=scale, package_metropolis=metropolis, issue_id, issue_title)
    
    rm(jj)
  }
  init()
  # refresh_test<-function(){
  #   dt_Protocols <<- "test"
  # }
  #self$jj<-jj
  
  # NOTA: verificato che se si espongono come variabili di istanza
  # dopo il refresh non si aggiornano le tabelle esposte.
  # Bisogna usare dei metodi getter, purtroppo perdendo comodità.
  #
  self$dt_Protocols<-function(){dt_Protocols}
  self$dt_Issues<-function(){dt_Issues}
  self$dt_SemanticPackages<-function(){dt_SemanticPackages}
  self$dt_Dynamics<-function(){dt_Dynamics}
  self$dt_Concepts<-function(){dt_Concepts}
  self$dt_Keywords<-function(){dt_Keywords}
  self$m2m_ProtocolScaleConcepts<-function(){m2m_ProtocolScaleConcepts}
  self$m2m_DynamicsConcepts<-function(){m2m_DynamicsConcepts}
  
  self$m2m_IssuesSemanticPackages<-function(){m2m_IssuesSemanticPackages}
  self$mm2mm_DynamicsSemanticPackagesIssues<-function(){mm2mm_DynamicsSemanticPackagesIssues}
  
  # self$semanticPackagesByMetropolisName<-function(metropolisName){
  #   m2m_IssuesSemanticPackages %>% 
  #     dplyr::filter(str_detect(metropolis,fixed(!!metropolisName, ignore_case=TRUE))) %>% 
  #     dplyr::mutate(package_id, title=paste(issue_title, scale)) %>% 
  #     dplyr::select(-issue_id, -issue_title) 
  # }
  
  self$dynamicsByMetropolisName<-function(metropolisName){
    mm2mm_DynamicsSemanticPackagesIssues %>% 
      dplyr::filter(str_detect(package_metropolis,fixed(!!metropolisName, ignore_case=TRUE)))
  }
  
  # pass to this method the result of dynamicsByMetropolisName
  self$listForDynamicsSelectize<-function(input){
    #input <- dynamicsByMetropolisName(metropolisName)
    # per farlo in un solo passaggio posso creare una nuova colonna con questa funzione di appoggio
    my_fx<-function(tb){tb$dynamic_id %>% set_names(tb$dynamic_title)}
    
    nested<-input %>% 
      as_tibble() %>% 
      dplyr::select(packageTitle, package_id, dynamic_title, dynamic_id) %>% 
      group_by(packageTitle, package_id) %>% 
      tidyr::nest() 
    
    nested_with_namedIntegerColumn<-nested %>% mutate(dd=map(data,my_fx)) 
    
    listaFinale<-
      nested_with_namedIntegerColumn %>% pull(dd) %>% set_names(nested_with_namedIntegerColumn %>% pull(packageTitle) )
    
    return(listaFinale)
  }
  
  # do not export the method by perspective by package: it is confusing. We use it only internally
  #self$perspectivesByPackageId<-perspectivesByPackageId # DEPRECATED export
  self$perspectivesByDynamicId<-perspectivesByDynamicId
  
  # DO NOT EXPORT: use the beanWithPerspectivesByDynamicId_tibble
  #self$beanFromDynamicId<-beanFromDynamicId # DEPRECATED export
  #self$beanFromPerspectiveId<-beanFromPerspectiveId # DEPRECATED export
  
  self$beanWithPerspectivesByDynamicId_tibble<-beanWithPerspectivesByDynamicId_tibble
  
  
  self$is_reading_online<-function(){return(read_online)}
  self$set_read_online<-function(online){read_online<<-online}
  self$refresh<-function(online=read_online){
    if(online!=read_online)
      read_online<<-online
    init()
  }
  
  # self$refresh<-init
  # # sets internal variable read_online, in order to be able to change it in the instance at runtime
  # self$setOnline<-function(online=TRUE){
  #   read_online<<-online #TODO: CHECK
  # }
  #self$refresh_test<-refresh_test
  
  return(self)
}

if(!FALSE){
  # create offline object instance
  glossary <- getGlossary(read_online = TRUE)
}
if(FALSE){
  
  ### some workout...
  # look at tables
  glossary$dt_Protocols() %>% as_tibble() %>% View()
  
  # refresh object by reading online
  #glossary$refresh()
  
  glossary$dt_SemanticPackages()
  
  # example: output a modified perspective table adding the package_id column
  glossary$dt_Dynamics()
  #glossary$perspectivesByPackageId(1) # DEPRECATED - internal use only
  #persp1<-glossary$perspectivesByPackageId(1) %>% dplyr::select(-dynamic_title) #%>% as_tibble()
  dyn<-glossary$dt_Dynamics() #%>% as_tibble()
  
  #glossary$perspectivesByPackageId(1) %>% class() 
  #glossary$perspectivesByPackageId(1) %>% typeof()
  glossary$dt_Dynamics() %>% class()
  glossary$dt_Dynamics() %>% typeof()
  
  # glossary$beanFromDynamicId(11) %>% as_tibble() %>% View() # DEPRECATED (now for internal use only) - use beanWithPerspectivesByDynamicId_tibble
  # glossary$beanFromPerspectiveId() # DEPRECATED (now for internal use only) - use beanWithPerspectivesByDynamicId_tibble
  
  ########### HOWTO USE FOR THE APP ##########
  ##### step 1: given a metropolis name obtain the available DYNAMICS (at differente scales)
  metropolis = "guadalajara"
  #glossary$semanticPackagesByMetropolisName(metropolisName = metropolis) # DEPRECATED - use dynamicsByMetropolisName
  glossary$dynamicsByMetropolisName(metropolis) 
  # -> columns are: 
  #   dynamic_id, dynamic_title, dynamic_conceptCount, 
  #   package_id, packageTitle, package_scale, package_metropolis, 
  #   issue_id, issue_title
  # [glossary$dynamicsByMetropolisName(metropolis) %>% as_tibble %>% colnames() %>% paste(collapse=", ")]
  # the previous is a convenience method to subset the following complete table (same columns)
  glossary$mm2mm_DynamicsSemanticPackagesIssues()
  
  ##### - step 2: given one DYNAMIC obtain some "meta info" about it and its perspectives
  
  #  chosen one dynamic by id we must obtain all the composing perspectives:
  # [dev note: lookup its semanticPackage, get all the perspective-dynamic in the package 
  #  (remember that one must filter by dynamic once obtained the whole set of perspectives by package), collect their ids
  #  ask one by one for their bean (bean of the dynamic and beans of its composing perspectives).
  #  The following method do this, resulting in all the perspectives for the given dynamic
  # ]
  # here we can obtain the titles of the dynamic and of each perspective
  glossary$perspectivesByDynamicId(21)
  # returned columns:
  #   dynamic_id, dynamic_title, perspective_id, perspective_title
  #
  # [obtain with: glossary$perspectivesByDynamicId(21) %>% as_tibble %>% colnames() %>% paste(collapse=", ")]
  
  
  ##### step 3:
  # obtain the "complete" bean for a given dynamic, that is:
  # a tibble with rows = concepts and one column per each perspective 
  #       containing an integer valued 1 if the concept is "on" in the perspective, 0 elsewhere.
  #       moreover the "selected" column contains the number of the perspectives in which the concept is on.
  # e.g. (21 is the id of one dynamic with more than one perspective)
  # NOTE: connects to glossary software
  
    glossary$beanWithPerspectivesByDynamicId_tibble(21) 
    glossary$beanWithPerspectivesByDynamicId_tibble(7) 
  
    #force reload with refresh=TRUE
    glossary$beanWithPerspectivesByDynamicId_tibble(7,refresh = TRUE) 
  
  # glossary$dt_Dynamics() %>% 
  #   left_join(glossary$dt_SemanticPackages()) %>% 
  #   dplyr::filter(conceptCount>0) %>% 
  #   left_join(glossary$dt_Issues() %>% select(issue_id, issue_title)) %>% 
  #   mutate(issueName=paste(issue_title,scale), package_scale=scale, package_metropolis=metropolis, dynamic_conceptCount=conceptCount) %>% 
  #   select(dynamic_id,dynamic_title,issue_id, issueName, package_scale=scale, package_metropolis=metropolis, dynamic_conceptCount, issue_title,scale)
  
  glossary$mm2mm_DynamicsSemanticPackagesIssues()
  # step 4: we must find the layers in TELLmeHub
  # we need a status vector containing:
  # - metropolis ? #--> the method for retrieving layers from tellmehub will need extent=-103.9361572265625,20.040450354169483,-102.49145507812501,21.580827113688514
  # - scale
  # then, for each concept in the bean we must retrieve one layer, 
  # looking through the available layers within the protocol maps in TELLmeHub at same scale and for the same city
  
  # e.g. API call to tellmehub: tellmehub.get-it.it/api/maps/extent=-103.9361572265625,20.040450354169483,-102.49145507812501,21.580827113688514
  # -> mappe protocollari di tellmehub relative alla città. 
  # in più dobbiamo passargli la scala
  # Noi dobbiamo selezionare in queste mappe protocollari tutti i layer che corrispondono a uno dei concetti nel fagiolone
  
  # data una scala
  # data una città
  # ho una serie di mappe protocollari in getit
  # lì dentro ho tanti layer, ognuno dei quali ha un concept_...
  #-> ottengo l'insieme di questi layer, concept_X, url WMS
  # BINGO!!
  
  
  beanDyn11 %>% filter(selected==TRUE)
  
  glossary$dt_SemanticPackages() %>% select(metropolis) %>% unique() %>% as_tibble()
  #glossary$semanticPackagesByMetropolisName("guadalajara")
  glossary$mm2mm_DynamicsSemanticPackagesIssues()
  glossary$dynamicsByMetropolisName("milan")
  beanDyn11<-glossary$beanWithPerspectivesByDynamicId_tibble(21)
  
}
if(FALSE){
  # #esempi uso in server.R
  metropolis="guadalajara"
  
  input<-glossary$dynamicsByMetropolisName(metropolis) 
  glossary$listForDynamicsSelectize(input)

}
