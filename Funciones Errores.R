

erroresExel <- function(frame,vars_select){
  errores<-c( '#DIV/0!','#N/A','#NAME?','#NULL!','#NUM!','#REF!','#VALUE!','#SPILL!',
       '#CALC!','#FIELD!','#BLOCKED!','#ERROR!','#¡DIV/0!','#¿NOMBRE?','#¡NULO!',
        '#¡NÚM!','#¡REF!','#¡VALOR!','#¡DESBORDAMIENTO!','#¡CALC!',
        '#¡CAMPO!','#¡BLOQUEADO!','#N/D','#¡NUM!','#NAME','**')
  vecColumnas<-apply(ungroup(frame), 2, function(col){
    colChar<-str_squish(toupper(as.character(col)))
    if(any(errores %in% colChar)){
      which( colChar %in% errores)
    }else{
      "sin error"
    }
  })
  vecColumnas<-subset(vecColumnas,vecColumnas!="sin error")
  vecColumnas<-unlist( vecColumnas )
  colsSelect<-unique(str_sub(names(vecColumnas),1,-2))
  comentarios<-data.frame()
  if(length(vecColumnas)>0) {
    for(i in 1:length(vecColumnas)){
      
      iter<- frame %>%
        ungroup() %>%
        mutate(campo=names(vecColumnas)[i]) %>%
        mutate(campo=gsub("[0-9]","",campo)) %>% 
        mutate(fila=row_number()+1) %>%
        filter(row_number()==vecColumnas[i]) %>%
        dplyr::select(fila,c(vars_select),campo)
      comentarios<-bind_rows(list(comentarios,iter))
    }
    
    comentarios %>% 
      arrange(fila) %>% 
      unique() %>%
      group_by(fila) %>% 
      mutate(coment=paste0("error formula excel en campo(s) ",
                           paste0(unique(campo),collapse = ", ") ) )
    
  }else{
    NULL
  }
} 

faltaInformacion <- function(frame,selecColumnas=c()){
  
  vecColumnas<-apply(ungroup(frame),
                     2,
                     function(col){
                       colChar<-str_squish(toupper(as.character(col)))
                       if(any(is.na(col))) {
                         which( (any(nchar(col) < 2) | is.na(col)))
                       }else{
                         "sin error"
                       }
                     })
  
  vecColumnas<-subset(vecColumnas,vecColumnas!="sin error")
  vecColumnas<-unlist( vecColumnas )
  colsSelect<-unique(str_sub(names(vecColumnas),1,-2))
  comentarios<-data.frame()
  if(length(vecColumnas)>0) {
    for(i in 1:length(vecColumnas)){
      
      iter<- frame %>%
        ungroup() %>%
        mutate(campo=names(vecColumnas)[i]) %>%
        mutate(campo=gsub("[0-9]","",campo)) %>% 
        mutate(fila=row_number()+1) %>%
        filter(row_number()==vecColumnas[i]) %>%
        dplyr::select(fila,c(selecColumnas),campo)
      comentarios<-bind_rows(list(comentarios,iter))
    }
    
    comentarios %>% 
      arrange(fila) %>% 
      unique() %>%
      group_by(fila) %>% 
      mutate(coment=paste0("Falta informacion en ", paste0(unique(campo),collapse = ", ")))
      
  }else{
    NULL
  }

}



encontrarPatron <- function(frame,patron){
  lista <- apply(frame,2,function(x){
    min( min( which(grepl(patron,x,ignore.case = T)),na.rm=T),99999999,na.rm = T)
  })
  columnas<-names(lista)
  
  resFrame<-data.frame(columnas=columnas,
                       fila=unname(lista)) %>% 
    mutate(fila=as.numeric(fila)) %>% 
    filter(fila < (nrow(frame)-10)) %>% 
    arrange(fila)
  
 return(resFrame)
}

