

erroresExel <- function(frame){
  errores<-c("#N/A","#N/D","#¡DIV/0!", "#¡VALOR!", "#VALUE!", "#¡REF!", "#¡NUM!", "#¿NOMBRE?", "#¡NULO!","**")
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
        dplyr::select(fila,identificacion,nombres_y_apellidos,empresa,campo)
      comentarios<-bind_rows(list(comentarios,iter))
    }
    
    comentarios %>% 
      arrange(fila) %>% 
      unique() %>%
      group_by(fila) %>% 
      summarise(identificacion=unique(identificacion)[1],
                nombres_y_apellidos=unique(nombres_y_apellidos)[1],
                empresa=unique(empresa)[1],
                coment=paste0("error formula excel en campo(s) ", paste0(campo,collapse = ", "))
      )
  }else{
    NULL
  }
} 

faltaInformacion <- function(frame,selecColumnas=c()){
  
  vecColumnas<-apply(ungroup(frame) %>% dplyr::select(any_of(selecColumnas)),
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
        dplyr::select(fila,numero_de_personal,apellidos_y_nombres,campo)
      comentarios<-bind_rows(list(comentarios,iter))
    }
    
    comentarios %>% 
      arrange(fila) %>% 
      unique() %>%
      group_by(fila) %>% 
      summarise(numero_de_personal=unique(numero_de_personal)[1],
                apellidos_y_nombres=unique(apellidos_y_nombres)[1],
                coment=paste0("Falta informacion en ", paste0(campo,collapse = ", "))
      )
  }else{
    NULL
  }
}