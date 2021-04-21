update_ic <- function(base_doc, instructions_tmp){  
  
  
  no3_frac = c(0.4,0.2,0.1,0.1,0.1, 0.05, 0.05, 0, 0, 0)
  #----------------------------------------------------------------------------------------------------
  # UPDATE THE BASE FILE
    sub_node <- xml_find_all(base_doc, '//Sample/NO3')
    vector <- as.character( instructions_tmp$n_target * no3_frac)
    
    counter = 1
    for(node_layer in xml_children(sub_node)){
      # node_layer = 1
      xml_text(node_layer) <- vector[counter]
      counter = counter + 1
    }# end node_layer
  
  
  return(base_doc)
  }