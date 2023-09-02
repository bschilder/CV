not_empty <- function(rc){
  !(is.na(rc) || rc=="")
}

icon_dict <- function(items=NULL,
                      dict=list(education="graduation-cap",
                                skills="check",
                                publications="file-text",
                                preprints="file-edit",
                                acknowledgements="file",
                                reviewerships="file",
                                invited_talks="person-chalkboard",
                                conference_talks="person-chalkboard",
                                posters="person-chalkboard",
                                experience="suitcase",
                                teaching="chalkboard-teacher",
                                packages="wrench",
                                # web_apps="desktop",
                                websites="computer",
                                databases="database",
                                grants="dollar",
                                awards="award",
                                affiliations="building-columns",
                                data_visualisation="circle-nodes",
                                extracurricular="icons"
                      ),
                      as_fa=FALSE,
                      as_icon=FALSE,
                      as_toc=FALSE,
                      collapse="<br>",
                      icon_width="12px"){

  # fontawesome:::alias_tbl[grepl("file",fontawesome:::alias_tbl$name),]
  if(isTRUE(as_toc) && isFALSE(as_icon)){
    message("as_fa and as_icon must both be TRUE when as_toc is TRUE. ",
            "Setting as_fa=TRUE and as_icon=TRUE.")
  }
  if(isTRUE(as_fa)){
    dict <- lapply(dict,function(x){ paste0("fa fa-",x)})
  }
  if(isTRUE(as_icon)){
    # dict <- lapply(dict,function(x){paste0("<i class=",shQuote(x),"></i>")})
    dict <- lapply(dict, function(x){fontawesome::fa(x,width = icon_width)})

  }
  if(isTRUE(as_toc)){
    # <i class='fa fa-award'></i> [Awards](#awards)
    dict <- lapply(stats::setNames(names(dict),
                                   names(dict)),
                   function(nm){
      paste0(dict[[nm]]," [",
             gsub("_"," ",stringr::str_to_title(nm)),"](#",nm,")")
    })
  }
  if(!is.null(items)){
    dict <- dict[items]
  }
  if(!is.null(collapse)){
    return(paste(dict,collapse = collapse))
  } else {
    return(dict)
  }
}




years_experience <- function(file=here::here("data","experience.csv"),
                             types="research"){

  Type <- NULL;
  dt <- data.table::fread(file)
  if(!is.null(types)){
    dt <- dt[Type %in% types,]
  }
  as.integer(format(Sys.Date(),"%Y")) - min(dt$StartYear, na.rm = TRUE)
}

n_tools <- function(file=here::here("data","tools.csv"),
                    types=NULL){
  Type <- NULL;
  dt <- data.table::fread(file)
  if(!is.null(types)){
    dt <- dt[Type %in% types,]
  }
  nrow(dt)
}

n_packages <- function(file=here::here("data","tools.csv"),
                       types=c("package","web app")){
  n_tools(file = file,
                 types = types)
}

n_webapps <- function(file=here::here("data","tools.csv"),
                      types="web app"){
  n_tools(file = file,
                 types = types)
}

n_websites <- function(file=here::here("data","tools.csv"),
                      types="website"){
  n_tools(file = file,
                 types = types)
}

n_web <- function(){
  sum(n_webapps(), n_websites(), na.rm = TRUE)
}

n_databases <- function(file=here::here("data","tools.csv"),
                        types="database"){
  n_tools(file = file,
                 types = types)
}


n_rpackages <- function(file=here::here("data","tools.csv"),
                        types="package",
                        languages="R"){
  Type <- Language <- NULL;
  dt <- data.table::fread(file)
  if(!is.null(types)){
    dt <- dt[Type %in% types,]
  }
  if(!is.null(languages)){
    dt <- dt[Language %in% languages,]
  }
  nrow(dt)
}

n_publications <- function(file=here::here("data","publications.csv"),
                           types="publication"){
  Type <- NULL;
  dt <- data.table::fread(file)
  if(!is.null(types)){
    dt <- dt[Type %in% types,]
  }
  nrow(dt)
}

n_posters <- function(file=here::here("data","publications.csv"),
                           types="poster"){
  Type <- NULL;
  dt <- data.table::fread(file)
  if(!is.null(types)){
    dt <- dt[Type %in% types,]
  }
  nrow(dt)
}

n_talks <- function(file=here::here("data","talks.csv"),
                    types=NULL){
  Type <- NULL;
  dt <- data.table::fread(file)
  if(!is.null(types)){
    dt <- dt[Type %in% types,]
  }
  nrow(dt)
}

n_grants <- function(file=here::here("data","grants.csv"),
                     types="grant",
                     roles=c("Primary applicant","Co-applicant")){
  Type <- Role <- NULL;
  dt <- data.table::fread(file)
  if(!is.null(types)){
    dt <- dt[Type %in% types,]
  }
  if(!is.null(roles)){
    dt <- dt[tolower(Role) %in% tolower(roles),]
  }
  nrow(dt)
}

parse_location <- function(r,
                           add_link=TRUE){

  txt <- paste0(
         if(not_empty(r$City)) paste0(r$City,", "),
         if(not_empty(r$State)) paste0(r$State,", "),
         if(not_empty(r$Country)) r$Country
  )
  baseurl <- "https://www.google.com/maps/place/"
  if(isTRUE(add_link)){
    if(r$Country=="Earth"){
      link <-  paste0("https://www.google.com/maps/",
                      "@47.8287853,41.5077906,22973464m/data=!3m1!1e3")
    } else {
      link <- gsub(" +","+",paste0(baseurl,txt))
    }
    txt2 <- paste0("<a href=",shQuote(link),">",txt,"</a>")
    return(txt2)
  } else {
    return(txt)
  }
}

parse_daterange <- function(r){
  if(!not_empty(r$StartYear)){
    r$EndYear
  } else if(!not_empty(r$EndYear)){
    r$StartYear
  } else if(r$EndYear==r$StartYear){
    return(r$EndYear)
  } else {
    paste(r$EndYear,"-",r$StartYear)
  }
}

parse_bullets <- function(r,
                          concise=FALSE){

  bullet_cols <- grep("Bullet_",names(r), value = TRUE)
  bullets <- as.list(r[,bullet_cols,with=FALSE])
  bullets <- bullets[bullets!="" & !is.na(bullets)]
  paste0(if(isTRUE(concise))"::: concise\n",
         paste("-",bullets, collapse = "\n"),
         if(isTRUE(concise))"\n:::"
  )
}

parse_news <- function(news,
                       title="News",
                       icon="newspaper"){
  # news <- dt$Comments[[16]]
  ctxt <- paste("-",trimws(strsplit(news,";")[[1]]),collapse = "<br>")
  if(!is.null(title)){
    ctxt <- paste0("**",
                   if(!is.null(icon)) fontawesome::fa(icon)," ",
                   title,
                   "**","<br>\n",ctxt)
  }
  return(ctxt)
}

# table_of_contents <- function(file=here::here("CV.Rmd")){
#   lines <- readLines(file)
#
# }

icon_link <- function(link,
                      text=link,
                      icon){
  paste0("<a href=",shQuote(link)," target='_blank'>",
         "<i class=",shQuote(icon),"></i> ",
         text,"</a>")
}


parse_education <- function(file=here::here("data","education.csv")){
  # ### Beijing University of Chemical Technology
  #
  # B.S. in Information and Computing Sciences
  #
  # Beijing, China
  #
  # 2010
  #
  # Thesis: Dyadic wavelet and its application in edge detection

  dt <- data.table::fread(file)
  txt <- lapply(seq_len(nrow(dt)), function(i){
    r <- dt[i,]
    paste(
      paste0("### ",r$Institution),
      paste0("**",r$Degree,"**: ",r$Program,"; ",r$Focus),
      parse_location(r=r),
      r$EndYear,
      if(r$Thesis!="") paste("**Thesis**:",r$Thesis),
      sep = "\n\n"
    )
  }) |> paste(collapse ="\n\n")
  cat(txt)
}


parse_publications <- function(file=here::here("data","publications.csv"),
                               name="BM Schilder",
                               types=NULL){
  # ### ESCRT-0 complex modulates Rbf mutant cell survival...
  #
  # J Cell Sci. 2016 May 15;129(10):2075-84.
  #
  # N/A
  #
  # 2016
  #
  # Sheng Z, **Yu L**, Zhang T, Pei X, Li X, Zhang Z and Du W.
  Type <- NULL;
  dt <- data.table::fread(file)
  if(!is.null(types)){
    dt <- dt[Type %in% types,]
  }
  txt <- lapply(seq_len(nrow(dt)), function(i){
    r <- dt[i,]
      paste(
        paste("###",r$Title),
        paste0("*",r$Journal,"* (",r$Year,") ",
               r$Volume,
               if(not_empty(r$Number)) paste0("(",r$Number,")"),
               if(not_empty(r$Pages)) paste0(":",r$Pages),
               if(any(r[,c("Volume","Number","Pages")]!=""))"; ",
               r$Link
        ),
        "N/A",
        r$Year,
        paste(
          gsub(name,paste0("**",name,"**"),r$Authors),
          if(not_empty(r$Comments)) parse_news(r$Comments),
          sep="<br>"
        ),
        sep = "\n\n"
      )
  })
  cat(paste(txt, collapse ="\n\n"))
}


parse_talks <- function(file=here::here("data","talks.csv"),
                        types=NULL){
  # ### ESCRT-0 complex modulates Rbf mutant cell survival...
  #
  # J Cell Sci. 2016 May 15;129(10):2075-84.
  #
  # N/A
  #
  # 2016
  #
  # Sheng Z, **Yu L**, Zhang T, Pei X, Li X, Zhang Z and Du W.
  Type <- NULL;
  dt <- data.table::fread(file)
  if(!is.null(types)){
    dt <- dt[Type %in% types,]
  }
  txt <- lapply(seq_len(nrow(dt)), function(i){
    r <- dt[i,]
    paste(
      paste("###",r$Title),
      paste(if(not_empty(r$Event)) r$Event,
            if(not_empty(r$Department)) r$Department,
            if(not_empty(r$Institution)) r$Institution,
            sep="<br>"
      ),
      "N/A",
      r$Year,
      if(not_empty(r$Comments)) r$Comments,
      sep = "\n\n"
    )
  })
  cat(paste(txt, collapse ="\n\n"))
}


parse_experience <- function(file=here::here("data","experience.csv"),
                             types=NULL,
                             concise=FALSE){
  # (Research)
  # ### Data Scientist, intern
  #
  # SupStat Inc.
  #
  # Beijing, China
  #
  # 2011 - 2014
  #
  # ::: concise
  # - Taught R language to beginners.
  # - Wrote Shiny app demos.
  # - Converted statistical tutorials from SPSS to R language.
  # :::

  # (Teaching)
  # ### Introduction to R Language for Beginners.
  #
  # Instructor of R and Data Mining Training Courses at SupStat Inc.
  #
  # Beijing, China
  #
  # 2014


  Type <- NULL;
  dt <- data.table::fread(file)
  if(!is.null(types)){
    dt <- dt[Type %in% types,]
  }
  txt <- lapply(seq_len(nrow(dt)), function(i){
    r <- dt[i,]
    if(r$Type=="teaching"){
      paste(
        paste("###",r$Position),"\n",
        paste0(if(not_empty(r$Institution)) r$Institution else "-",
               if(not_empty(r$Department)) paste0(" (",r$Department,")")
        ),
        parse_location(r = r),
        parse_daterange(r = r),
        parse_bullets(r = r,
                      concise = concise),
        sep = "\n\n"
      )
    }else if(r$Type=="extracurricular"){
      paste(
        paste("###",r$Position),
        parse_bullets(r = r,
                      concise = concise),
        parse_location(r=r),
        ".",
        parse_daterange(r=r),
        sep = "\n\n"
      )
    } else if(r$Type=="data visualisation"){
      paste(
        paste0("### ",
              "[",r$Position,
              # " <img src=",shQuote(r$Link)," height='50' style='valign:top;'>",
              "](",r$Link,")"
              ),
        paste0(if(not_empty(r$Institution)) r$Institution else "N/A",
               if(not_empty(r$Department))paste0(" (",r$Department,")")
        ),
        parse_location(r=r),
        parse_daterange(r=r),
        parse_bullets(r = r,
                      concise = concise),
        sep = "\n\n"
      )
    }else {
      paste(
        paste("###",r$Position),
        paste0(if(not_empty(r$Institution)) r$Institution else "N/A",
               if(not_empty(r$Department))paste0(" (",r$Department,")")
        ),
        parse_location(r=r),
        parse_daterange(r=r),
        parse_bullets(r = r,
                      concise = concise),
        sep = "\n\n"
      )
    }
  }) |> gsub(pattern = "\n\n\n\n",replacement = "\n\n")
  cat(paste(txt, collapse ="\n\n"))
}



parse_tools <- function(file=here::here("data","tools.csv"),
                           types=NULL,
                           add_index=TRUE){
  # ### Data Scientist, intern
  #
  # SupStat Inc.
  #
  # Beijing, China
  #
  # 2014
  #
  # ::: concise
  # - Taught R language to beginners.
  # - Wrote Shiny app demos.
  # - Converted statistical tutorials from SPSS to R language.
  # :::

  Type <- NULL;
  dt <- data.table::fread(file)
  if(!is.null(types)){
    dt <- dt[Type %in% types,]
  }
  txt <- lapply(seq_len(nrow(dt)), function(i){
    r <- dt[i,]
    paste(
      paste("###",r$Name),
      paste(
        r$Title,
        if(not_empty(r$GitHub)){
          icon_link(link = r$GitHub,
                    icon="fa fa-github")
        },
        if(not_empty(r$Link)){
          icon_link(link = r$Link,
                    icon="fa fa-link")
        },
        if(not_empty(r$PaperLink)){
          icon_link(link = r$PaperLink,
                    icon="fa fa-file")
        },
        sep="<br>"
      ),
      "N/A",
      if(isTRUE(add_index)) paste0(i,"\\.") else "N/A",
      sep = "\n\n"
    )
  }) |> gsub(pattern = "<br><br>",replacement = "<br>")
  cat(paste(txt, collapse ="\n\n"))
}


parse_profile <- function(file=here::here("data","profile.csv"),
                          types=NULL,
                          concise=FALSE,
                          sep="\n\n",
                          collapse = "<br>",
                          div="h4",
                          img_width="100px",
                          icon_width="12px",
                          prefix=NULL){
  # - <i class="fa fa-envelope"></i> brian_schilder@alumni.brown.edu
  # - <i class="fa fa-linkedin"></i> [LinkedIn](https://twitter.com/BMSchilder)
  # - <i class="fa fa-github"></i> [GitHub](https://github.com/bschilder)
  # - <i class="fa fa-twitter"></i> [Twitter](https://www.linkedin.com/in/brian-schilder)
  # - <i class="fa fa-globe"></i> [Professional Website](https://bschilder.github.io/BMSchilder)
  # - <i class="fa fa-globe"></i> [Lab Website](https://www.neurogenomics.co.uk/)
  # - <i class="fa fa-phone"></i> **US**: <a href="tel:+1 908-268-9859">+1 908-268-9859</a>
  #  - <i class="fa fa-phone"></i> **UK**: <a href="tel:+44 073-0653-7736">+44 073-0653-7736</a>
  Type <- NULL;
  dt <- data.table::fread(file)
  if(!is.null(types)){
    dt <- dt[Type %in% types,]
  }
  txt <- lapply(seq_len(nrow(dt)), function(i){
    r <- dt[i,]
    if(!grepl("^\\./",r$Icon)){
      paste(
        fontawesome::fa(r$Icon,width=icon_width),
        if(r$Type=="phone"){
          paste0(r$Text,
                 "<br>",
                 "<a href=",shQuote(paste0("tel:",r$Link)),">",
                 r$Link,
                 "</a>")
        } else if(not_empty(r$Text)){
          paste0("[",r$Text,"](",r$Link,")")
        } else {
          r$Link
        }
      )
    } else {
      paste0(
        prefix,
        "<a href=",shQuote(r$Link)," target='_blank' class='affiliate'>",
        "<img src=",shQuote(r$Icon),"alt=",shQuote(r$Text),
        "width=",shQuote(img_width),">",
        "</a>",
        sep,
        "\n",
        parse_bullets(r = r, concise = concise),
        paste(c(" "," ","N/A"), collapse = collapse),
        sep
        )
    }
  }) |> paste(collapse = collapse)
  if(isTRUE(concise)){
    return(
      cat(
        paste(
          "::: concise\n",
          txt,
          ":::"
        )
      )
    )
  } else {
    if(!is.null(div)){
      txt <- paste0("<",div,">",txt,"</",div,">")
    }
    return(cat(txt))
  }

}


parse_grants_totals <- function(dt){
  Amount <- NULL;
  dt[,unit:=ifelse(grepl("£",Amount),"£",
                   ifelse(grepl("\\$",Amount),"$",NA)
                   )]
  suppressWarnings(
    dt[,Amount2:=as.integer(gsub("£|\\$|,","",Amount))]
  )
  #### Convert currency ####
  dt[,Amount2:=ifelse(unit=="$",Amount2,Amount2*1.24)]
  sum(dt$Amount2, na.rm = TRUE)
}

parse_grants <- function(file=here::here("data","grants.csv"),
                         types=NULL,
                         add_totals=FALSE){
  # devoptera::args2vars(parse_grants,  run_source_all = FALSE)

  # ### ###Imperial UK Research Institute Impact Acceleration Account
  #
  # SupStat Inc.
  #
  # Beijing, China
  #
  # 2014
  #
  # ::: concise
  # - Taught R language to beginners.
  # - Wrote Shiny app demos.
  # - Converted statistical tutorials from SPSS to R language.
  # :::

  Type <- Role <- NULL;
  dt <- data.table::fread(file)
  if(isTRUE(add_totals)){
    txt_totals <-
      paste(
        paste(
          paste0("### **Total (all grants)**: ",
                 "$",
                 formatC(
                   parse_grants_totals(dt = dt[Type=="grant",]),
                   big.mark = ",",
                   format = "d"
                 )),
          paste0("**Total (as primary applicant)**: ",
                 "$",
                 formatC(
                   parse_grants_totals(dt = dt[Type=="grant" &
                                               Role=="Primary applicant",]),
                   big.mark = ",",
                   format = "d"
                 )
          ),
          sep="<br>"
        ),
        paste(c(rep("N/A",2),"<hr>",rep("N/A",1)),collapse = "\n\n"),
        sep="\n\n"
      )
    cat(txt_totals,"\n\n")
  }
  if(!is.null(types)){
    dt <- dt[Type %in% types,]
  }
  txt <- lapply(seq_len(nrow(dt)), function(i){
    r <- dt[i,]
    paste(
      paste(
        "###",
        paste(
          if(not_empty(r$GrantName)) r$GrantName,
          if(not_empty(r$Source)) r$Source,
          sep = ",<br>"
        )|> gsub(pattern="^[,<br>]",replacement="",)
      ),
      if(not_empty(r$Project))paste0("**Project**: ",shQuote(r$Project)),
      "N/A",
      parse_daterange(r=r),
      paste(
        ":::  concise",
        if(not_empty(r$Comments)) paste0(parse_news(r$Comments),"\n"),
        if(not_empty(r$Role))paste0("- **Role**: ",r$Role),
        if(not_empty(r$PI) && r$Type!="award")paste0("- **PI**: ",r$PI),
        if(not_empty(r$Amount))paste0("- **Amount**: ",r$Amount),
        ":::",
        sep = "\n"
      ),
      sep = "\n\n"
    )
  }) |>
    gsub(pattern="<br><br>",replacement="<br>") |>
    gsub(pattern="### <br>",replacement="### ")

  cat(paste(txt, collapse ="\n\n"))
}


build_skill_bars <- function(percent,
                             title=""){
  bar_color <- "#969696"
    bar_background <- "#d9d9d9"
      paste(
        "<div class = 'skill-bar'",
        "style = \"background:linear-gradient(to right,",
        paste0("{",bar_color,"}"),
        paste0("{",percent,"}%,"),
        paste0("{",bar_background,"}"),
        paste0("{",percent,"}%"),
        "100%)\" >",
        paste0("{",title,"}"),
        "</div>"
      )
}

parse_skills <- function(file=here::here("data","skills.csv"),
                         types=NULL){

  Title <- Type <- Level <- LevelMax <- Percent <- NULL;
  dt <- data.table::fread(file)
  dt <- dt[,Percent:=round(100*Level/LevelMax)]
  if(!is.null(types)){
    dt <- dt[Type %in% types,]
  }
  txt <- lapply(unique(dt$Group), function(group){
    rg <- dt[Group==group,]
    summary <- rg[Title=="Summary",]$Description
    rg <- rg[Title!="Summary",]
    mean_percent <- round(mean(rg$Percent,na.rm = TRUE),1)
    paste(
      paste("###",group),
      paste(summary,
            # build_skill_bars(percent = mean_percent,
            #                  title = group)
            sep="<br>"
            ),
      "N/A",
      c("::: concise",
        lapply(seq_len(nrow(rg)), function(i){
          r <- rg[i,]
          # build_skill_bars(dt = r)
          paste0("- **",r$Title,"**: ",r$Description)
        }),
        ":::"
      )|> paste(collapse = "\n"),
      paste(rep("N/A",1),collapse = "\n\n"),
      sep = "\n\n",
      collapse = "\n\n"
    )
  })
  #### Add special variables ####
  txt <- gsub("{years_experience}",
              paste0("[**",years_experience(),"**](#experience)"),
              txt, fixed=TRUE)
  txt <- gsub("{n_rpackages}",
              paste0("[**",n_rpackages(),"**](#software)"),
              txt, fixed=TRUE)
  txt <- gsub("{n_publications}",
              paste0("[**",n_publications(),"**](#publications)"),
              txt, fixed=TRUE)
  txt <- gsub("{n_posters}",
              paste0("[**",n_posters(),"**](#posters)"),
              txt, fixed=TRUE)
  txt <- gsub("{n_grants}",
              paste0("[**",n_grants(),"**](#grants)"),
              txt, fixed=TRUE)
  txt <- gsub("{n_websites}",
              paste0("[**",n_websites(),"**](#websites)"),
              txt, fixed=TRUE)
  txt <- gsub("{n_webapps}",
              paste0("[**",n_webapps(),"**](#webapps)"),
              txt, fixed=TRUE)
  txt <- gsub("{n_web}",
              paste0("[**",n_web(),"**](#webapps)"),
              txt, fixed=TRUE)
  cat(paste(txt,collapse = "\n\n"))
}

build_toc <- function(items=NULL,
                      add_div="h4",
                      collapse="<br>"){
  dict <- icon_dict(items = items,
                    as_icon = TRUE,
                    as_toc = TRUE,
                    collapse = collapse)
  if(!is.null(add_div)){
    dict <- paste0("<",add_div,">",dict,"</",add_div,">")
  }
  cat(dict)
}

build_footer <- function(add_github="https://github.com/bschilder/CV",
                         add_pagedown=FALSE,
                         add_date=TRUE,
                         sep="<br>"){

  # *This CV was made with [`pagedown`](https://github.com/rstudio/pagedown).*
  #
  # *Last updated: `r format(Sys.Date(),"%b-%d-%Y")`*
  txt <- paste(
    if(!is.null(add_github)){
      paste0("<a href=",shQuote(add_github),">",
             fontawesome::fa("github")," *CV source code*",
             "</a>"
      )
    },
    if(isTRUE(add_pagedown)){
      paste0("*Made with",
             "[`pagedown`](https://github.com/rstudio/pagedown).*")
    },
    if(isTRUE(add_date)){
      paste0(fontawesome::fa("calendar"),
             " *Updated ",format(Sys.Date(),"%b-%d-%Y"),"*")
    },
    sep=sep
  )
  txt <- paste0("<p style='color: rgba(0,0,0,0.5)'>",txt,"</p>")
  cat(txt)
}

build_summary <- function(items=c("years_experience_research",
                                  "n_publications",
                                  "n_preprints",
                                  "n_packages",
                                  "n_databases",
                                  "n_talks",
                                  "years_experience_teaching"),
                          plus = list(years_experience_research="+",
                                      n_publications="",
                                      n_preprints="",
                                      n_packages="",
                                      n_databases="",
                                      n_talks="",
                                      years_experience_teaching="+"),
                          collapse = "<br>"){
  # <i class='fa fa-suitcase'></i> [`r years_experience(types="research")`+ years of research experience.](#experience)
  # <i class='fa fa-file'></i> [`r n_publications()` peer-reviewed publications to date.](#publications)
  # <i class='fa fa-desktop'></i> [`r n_packages()` bioinformatics tools developed.](#software)
  # <i class='fa fa-chalkboard-teacher'></i> [`r years_experience(types = "teaching")`+ years of teaching/supervising experience.](#teaching)

  res <- lapply(stats::setNames(items,items),
                function(x){
                  if(x=="years_experience_research"){
                    paste(
                      icon_dict(items = "experience",
                                as_icon = TRUE),
                      paste0(
                        "[",years_experience(types="research"),
                        plus[[x]]
                      ),
                      "years of research](#experience)"
                    )
                  } else if (x=="n_publications"){
                    paste(
                      icon_dict(items = "publications",
                                as_icon = TRUE),
                      paste0(
                        "[",n_publications(),plus[[x]],
                        " publications",
                        "](#publications)"
                      )
                    )
                  } else if (x=="n_preprints"){
                    paste(
                      icon_dict(items = "preprints",
                                as_icon = TRUE),
                      paste0(
                        "[",n_publications(types = "preprint"),plus[[x]],
                        " preprints",
                        "](#preprints)"
                      )
                    )
                  } else if(x=="n_packages"){
                    paste(
                      icon_dict(items = "packages",
                                as_icon = TRUE),
                      paste0(
                        "[",n_tools(types = "package"),
                        plus[[x]]
                      ),
                      "software packages](#packages)"
                    )
                  } else if(x=="n_databases"){
                    paste(
                      icon_dict(items = "databases",
                                as_icon = TRUE),
                      paste0(
                        "[",n_tools(types = c("database","web app")),
                        plus[[x]]
                      ),
                      "databases & apps](#databases)"
                    )
                  } else if(x=="n_talks"){
                    paste(
                      icon_dict(items = "conference_talks",
                                as_icon = TRUE),
                      "[",n_talks(),
                      "talks](#talks)"
                    )
                  } else if(x=="years_experience_teaching"){
                    paste(
                      icon_dict(items = "teaching",
                                as_icon = TRUE),
                      paste0(
                        "[",years_experience(types='teaching'),
                        plus[[x]]
                      ),
                      "years of teaching & team management](#teaching)"
                    )
                  }
                })
  if(!is.null(collapse)){
    cat(paste(res,collapse = collapse))
  } else {
    return(res)
  }
}


plot_skills <- function(file=here::here("data","skills.csv"),
                        types=NULL){

  Title <- Type <- Level <- LevelMax <- Percent <- NULL;
  dt <- data.table::fread(file)
  dt <- dt[,Percent:=round(100*Level/LevelMax)]
  if(!is.null(types)){
    dt <- dt[Type %in% types,]
  }

  dt_plot <- data.table::copy(dt)[Title!="Summary",] |>
    data.table::setorderv(cols = c("Group","Title"))
  dt_plot$Title <- factor(dt_plot$Title,unique(dt_plot$Title), ordered = TRUE)
  ggplot2::ggplot(dt_plot,
                  ggplot2::aes(x=Title, y=Percent,
                               fill=Group)) +
    ggplot2::geom_bar(stat = "identity")  +
    ggplot2::coord_polar() +
    ggplot2::theme_bw() +
    ggplot2::scale_fill_viridis_d(alpha = 0.8)
}

build_network <- function(files=list.files(path = here::here("data"),
                                           pattern = ".csv$",
                                           full.names = TRUE),
                          min_count = 2,
                          min_nchar = 2,
                          layout = "layout_as_star",
                          center = tolower("BM Schilder"),
                          shape = "hexagon",
                          randomSeed = 2023,
                          save_path = NULL,
                          show_plot = TRUE,
                          export_type="png"){

  # templateR:::source_all(path = "code")
  # templateR:::args2vars(build_network)

  library(textnets)
  #### Convert CSVs to text ####
  dt <- lapply(stats::setNames(files,
                         gsub("\\.csv","",basename(files))),
                 function(f){
    txt <- gsub("_"," ",unname(unlist(as.list(data.table::fread(f)))))
    data.table::data.table(text=paste(txt[txt!="" & !is.na(txt)],
                                      collapse = " "))
  }) |> data.table::rbindlist(use.names = TRUE,
                              idcol = 'file')
  #### Prepare text ####
  prepped <- textnets::PrepText(dt,
                                groupvar = "file",
                                textvar = "text",
                                node_type ="words",
                                tokenizer = "words",
                                pos = "nouns",
                                remove_stop_words = TRUE,
                                compound_nouns = TRUE)
  #### Filter words ####
  prepped <- subset(prepped,
                      count>min_count &
                      nchar(prepped$lemma)>min_nchar)
  g <- textnets::CreateTextnet(tidytextobject = prepped)
  #### Add connectivity ####
  igraph::V(g)$degree <- igraph::degree(g, mode = "all")
  #### Cluster ####
  clusters <- igraph::cluster_louvain(g, resolution = 5)
  igraph::V(g)$cluster <- clusters$membership
  #### Node size #####
  size_var <- "degree"
  igraph::V(g)$value <- igraph::vertex_attr(g,size_var)
  if(center %in% names(igraph::V(g))){
    igraph::V(g)$value[which(names(igraph::V(g))==center)] <-
      max(igraph::V(g)$value)*100
  }
  #### Node color ####
  color_var <- "cluster"
  ncolors <- length(unique(igraph::vertex_attr(g,color_var)))
  my_palette <- pals::ocean.thermal(ncolors)
  igraph::V(g)$color <- my_palette[
    cut(igraph::vertex_attr(g,color_var) ,ncolors)
  ]
  #### Plot ####
  # net <- textnets::VisTextNet(text_network = g,
  #                             label_degree_cut = 0)
  # netd3 <- textnets::VisTextNetD3(text_network = g)
  visnet <- visNetwork::visIgraph(g,
                        layout = layout,
                        randomSeed = randomSeed,
                        center = center) |>
    visNetwork::visNodes(opacity = .5,
                         shape = shape,
                         shadow = list(enabled=TRUE,
                                       size=10),
                         font = list(strokeWidth=20,
                                     strokeColor="rgba(255,255,255,.5)"),
                         scaling=list(max=200,
                                      label=list(min=30,
                                                 maxVisible=2000,
                                                 max=100))) |>
    visNetwork::visEdges(color = list(opacity=.7),
                         # width=3,
                         # smooth = list(enabled=TRUE,
                         #               type="continuous",
                         #               roundness=.5
                         #               ),
                         dashes = FALSE) |>
    visNetwork::visExport(type = export_type,
                          name = paste0(gsub(" ","_",center),"_network"))|>
    visNetwork::visOptions(height = 700,
                           width = 1200)
  if(!is.null(save_path)){
    dir.create(dirname(save_path), showWarnings = FALSE, recursive = TRUE)
    visNetwork::visSave(graph = visnet,
                        file = save_path,
                        background = "transparent",#"rgba(0,0,0,0.9)",
                        selfcontained = TRUE)
  }
  if(isTRUE(show_plot)){
    methods::show(visnet)
  }
  return(visnet)
}


search_pubmed <- function(query){
  # query <- "'Brian Schilder'[aut] OR 'Brian M Schilder'[aut]"
  requireNamespace("pubmedR")
  requireNamespace("bibliometrix")
  requireNamespace("ggplot2")

  res <- pubmedR::pmQueryTotalCount(query = query)
  D <- pubmedR::pmApiRequest(query = query,
                             limit = res$total_count,
                             api_key = NULL)
  # M <- pubmedR::pmApi2df(D)
  M2 <- bibliometrix::convert2df(file = D,
                                 dbsource = "pubmed",
                                 format = "api")
  M2$ID_ <- M2$ID
  M2$ID <- paste(M2$TI, M2$AB, M2$ID, M2$DE)
  cs <- bibliometrix::conceptualStructure(M2,
                                          # minDegree = 1,
                                          # clust = 3,
                                          field = "ID")

  plot_df <- cs$docCoord |> tibble::rownames_to_column("id") |>
    merge(M2 |> dplyr::mutate(id=tolower(SR)), by="id") |>
    dplyr::mutate(
      label = paste(
        stringr::str_to_title(id),
                             shQuote(
        stringr::str_trunc(
          width = 50,
          stringr::str_to_sentence(TI)
          )
        ),
        sep="\n"
      ),
      Cluster=factor(Cluster)
    )

  ggplot(plot_df, aes(x=dim1, y=dim2,
                      color=Cluster,
                      size=contrib,
                      label=label)) +
    geom_point(alpha=.8) +
    scale_color_viridis_d(end = .8, option = "plasma") +
    # geom_label() +
    ggrepel::geom_label_repel(size=3, alpha=.8) +
    theme_bw()

  results <- bibliometrix::biblioAnalysis(M2)
  net <- bibliometrix::biblioNetwork(M2)
  # summary(results)
  return(list(query=query,
              raw=D,
              df=M2,
              analysis=results,
              network=net))
}

search_pubmed_fulltext <- function(query,
                                   target_terms=NULL){

  query <- "(AUTHOR:(Brian Schilder) OR AUTHOR:(Brian M Schilder))"
  mesh <- data.table::fread("~/Desktop/Geneshot_ontologies/data/ontologies/BioPortal/MESH.csv.gz")
  efo <- ontoProc::getEFOOnto()
  target_terms <- c(mesh$`Preferred Label`,
                    unname(efo$name))|>
    tolower()
  library(pluralize)

  res <- europepmc::epmc_search(paste(query,"OPEN_ACCESS:Y"))
  docs <- Map(europepmc::epmc_ftxt,na.omit(res$pmcid))
  txt <- Map(tidypmc::pmc_text, docs)
  # metadata <- Map(tidypmc::pmc_metadata, docs) |>
  #   data.table::rbindlist(use.names = TRUE,
  #                         idcol = "doc_id",
  #                         fill = TRUE)
  # table <- Map(tidypmc::pmc_table, docs)
  #### Collapse text ####
  dt <- data.table::rbindlist(
    txt,
    use.names = TRUE,
    idcol = "doc_id")[,list(
      text=text |> pluralize::singularize() |>
        tm::removePunctuation() |>
        tm::removeNumbers() |>
        paste(collapse = " ")
      ),
      by="doc_id"]
  stp <- tidytext::get_stopwords()$word
  #### TermDocumentMatrix ####
  tdm <-
    tm::DataframeSource(dt) |>
    tm::Corpus() |>
    tm::TermDocumentMatrix()
  #### TF-IDF ####
  counts <- data.table::rbindlist(
    txt,
    use.names = TRUE,
    idcol = "doc_id") |>
    tidytext::unnest_tokens(word, text) |>
    subset(!word %in% stp) |>
    dplyr::count(doc_id, word, sort = TRUE)
  if(is.null(target_terms)){
    target_terms <- counts$word
  }
  tfidf_target <- tidytext::bind_tf_idf(counts,
                                 term = "word",
                                 document = "doc_id",
                                 n = "n") |>
    dplyr::mutate(len=nchar(word)) |>
    dplyr::mutate(word=pluralize:::singularize(word)) |>
    subset(word %in% target_terms) |>
    subset(len > 2) |>
    dplyr::group_by(word) |>
    dplyr::summarise(n=sum(n),
                     tf_idf=mean(tf_idf)) |>
    dplyr::arrange(dplyr::desc(tf_idf))
  #### Compute term-term co-occurence ####
  X <- as.matrix(tdm[rownames(tdm) %in% head(tfidf_target$word,10000),])
  X_cor <- cor(t(X))
  X_cor <- X_cor + abs(min(X_cor, na.rm = TRUE))
  diag(X_cor) <- NA
  g <- tidygraph::as_tbl_graph(X_cor) |>
    tidygraph::activate(what = "edges")
  g <- g |>
    tidygraph::filter(weight>rev(
      quantile(data.frame(g)$weight,
               na.rm = TRUE,
               seq(0, 1, 0.1)
      )
    )[2]) |>
  tidygraph::filter(!is.na(weight))
  g <- g |> tidygraph::activate(what = "nodes")
  pal <- colorRamp2::colorRamp2(breaks = quantile(igraph::harmonic_centrality(g),
                                                  probs = seq(0,1,length.out=1000)),
                                colors = pals::viridis(1000),
                                transparency = .75)
  igraph::V(g)$color <- pal(igraph::harmonic_centrality(g))
  igraph::V(g)$value <- igraph::harmonic_centrality(g)^3
  # plot(g)
  vn <- visNetwork::visIgraph(g,
                        randomSeed = 2023,
                        # layout = "layout_with_kk"
                        ) |>
    visNetwork::visEdges(arrows = list(enable=FALSE))
  methods::show(vn)
  #### Return ####
  return(
    list(documents=dt,
         tfidf_target=tfidf_target,
         graph=g,
         plot=vn
         )
  )

  #### UMAP of documents ####
  # X_tfidf <- tidytext::cast_sparse(tfidf,
  #                                  row = "doc_id",
  #                                  column = "word",
  #                                  value = "tf_idf")
  # obj <- scKirby::process_seurat(obj = X_tfidf)
  # mod <- uwot::umap(as.matrix(X_tfidf),
  #                   n_neighbors = nrow(X_tfidf)-1)
  # umap_df <- merge(
  #  metadata,
  #  data.table::as.data.table(
  #    mod |> `colnames<-`(paste0("UMAP",seq_len(ncol(mod)))),
  #    keep.rownames = "doc_id"
  #  ))
  #
  # ggplot(umap_df, aes(x=UMAP1, y=UMAP2,
  #                     # color=Cluster,
  #                     # size=contrib,
  #                     label=stringr::str_wrap(Title,50))) +
  #   geom_point(alpha=.8) +
  #   scale_color_viridis_d(end = .8, option = "plasma") +
  #   # geom_label() +
  #   ggrepel::geom_label_repel(
  #     size=3, alpha=.8, max.overlaps = 30) +
  #   theme_bw()
}



