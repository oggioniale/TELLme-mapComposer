require(jqr)
require(httr)
require(dplyr)
require(jsonlite)
require(openssl)
#install.packages('dtplyr')
require(dtplyr)

# packages for web scraping
require(rvest)
require(stringr)
require(purrr)


source("accounts_private.R")

getGlossary <- function() {
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
      export <-
        httr::GET(url = url, add_headers("Authorization" = paste("Basic", token)))
      if (export$status_code != 200) {
        jsonunparsed <- readLines("./export_bar.json",-1)
        warning(paste0(
          "Unable to read latest glossary version: polimi software returned status code ",
          export$status_code, '. A cached version will be used')
        )
      }
      else{
        #export$content
        jsonunparsed <- httr::content(export, "text")
      }
      message('...done')
      return(jsonunparsed)
    }

  ## internal variables
  {
  jj<-NULL 
  # data tables
  dt_Protocols<-NULL
  dt_Issues<-NULL
  dt_SemanticPackages<-NULL
  dt_Dynamics<-NULL
  dt_Concepts<-NULL
  dt_Keywords<-NULL
  m2m_ProtocolScaleConcepts<-NULL
  m2m_DynamicsConcepts<-NULL
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
  do_Q<-function(q){
    jj %>% jqr::jq(as.character(q)) %>% 
      textConnection() %>% jsonlite::stream_in(simplifyDataFrame=TRUE) %>% 
      dtplyr::lazy_dt()
  }
  
  #### scrape web pages for entities not present in the polimi tellme glossary software json dump
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
  
  fagioloneWebByURL<-function(url, usr=TELLME_GLOSSARY_USER, pwd=TELLME_GLOSSARY_PASSWORD){
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
      warning("no script with concept selection found (maybe a protocol?)")
      return(bean)
    }
    s<-rvest::html_text(scripts[[1]])
    
    ids_selected<-stringr::str_match_all(s,stringr::regex("data-conceptid='(\\d+)'", multiline=TRUE))[[1]][,2] %>% 
      dplyr::as_tibble() %>% dtplyr::lazy_dt() %>% 
      rename(id=value) %>% dplyr::mutate(selected=1, concept_slug=paste0("concept_",id))
    
    bean %>% left_join(ids_selected) 
  }
  # # example usage:
  # dyn21url="http://www.tellme.polimi.it/tellme_apps/tellme/dynamic/21"
  # fagioloneWebByURL(dyn21url)
  
  # dynamic specific function
  fagioloneWebByDynamicId<-function(id){
    url=paste0("http://www.tellme.polimi.it/tellme_apps/tellme/dynamic/",id)
    fagioloneWebByURL(url)
  }
  # #example usage:
  # fagioloneWebByDynamicId(21)
  
  # perspective specific function
  fagioloneWebByPerspectiveId<-function(id){
    url=paste0("http://www.tellme.polimi.it/tellme_apps/tellme/perspective/",id)
    fagioloneWebByURL(url)
  }
  
  # fagioloneWebByPerspectiveId(26)
  
  # fagioloneWebByURL("http://www.tellme.polimi.it/tellme_apps/tellme/protocol/43")
  
  # extract for a given package the (one2many) table of dynamic_id <- perspective_id.
  perspectivesByPackageId<-function(package_id){
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
  
  # e.g.
  #perspectivesByPackageId(21)
  
  #######
  # initialize object
  init<-function(){
    jj <<- internal.getGlossarySoftwareJsonDump()
    
    paste0("...updating glossary object tables...")
    dt_Protocols <<- do_Q(q$protocols)
    dt_Issues<<-do_Q(q$issues)
    dt_SemanticPackages<<-do_Q(q$semanticPackages)
    dt_Dynamics<<-do_Q(q$dynamics)
    dt_Concepts<<-do_Q(q$concepts)
    dt_Keywords<<-do_Q(q$keywords)
    m2m_ProtocolScaleConcepts<<-do_Q(q$m2m_ProtocolScaleConcepts)
    m2m_DynamicsConcepts<-do_Q(q$m2m_DynamicsConcepts)
  }
  init()
  # refresh_test<-function(){
  #   dt_Protocols <<- "test"
  # }
  self$jj<-jj
  
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
  
  self$perspectivesByPackageId<-perspectivesByPackageId
  self$beanFromDynamicId<-fagioloneWebByDynamicId
  self$beanFromPerspectiveId<-fagioloneWebByPerspectiveId
    
  self$refresh<-init
  #self$refresh_test<-refresh_test
  
  return(self)
}

if(FALSE){
 # create object instance
 glossary <- getGlossary()

# look at tables
glossary$dt_Protocols()

# refresh object by reading online
#glossary$refresh()

glossary$dt_SemanticPackages()

# example: output a modified perspective table adding the package_id column
glossary$dt_Dynamics()
glossary$perspectivesByPackageId(1)
persp1<-glossary$perspectivesByPackageId(1) %>% dplyr::select(-dynamic_title) #%>% as_tibble()
dyn<-glossary$dt_Dynamics() #%>% as_tibble()

glossary$perspectivesByPackageId(1) %>% class() 
glossary$perspectivesByPackageId(1) %>% typeof()
glossary$dt_Dynamics() %>% class()
glossary$dt_Dynamics() %>% typeof()


# perspective è un tibble, dynamic è un lazy data.table.
# uno dei due prima del join va castato nel tipo dell'altro.
glossary$perspectivesByPackageId(1) %>% dtplyr::lazy_dt() %>% dplyr::left_join(glossary$dt_Dynamics())
glossary$perspectivesByPackageId(1) %>% dplyr::left_join(glossary$dt_Dynamics() %>% as_tibble())

# bea
glossary$beanFromDynamicId(11)
}
