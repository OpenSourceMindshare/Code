# Remove columns with an NA proportion greater than a specified value.
remove_na_columns <- function(df,max_na_prop=0.5){
  props <- lapply(df,function(x){
    sum(is.na(x))/length(x)
  })
  remove <- which(props>=max_na_prop)
  if(length(remove)!=0){
    return(df[,-remove])
  }else{
    return(df)
  }
}

# Replace NA in a vector with values depending upon variable class
replace_na <- function(x,int_replacement=0,char_replacement='Missing'){

  type <- class(unlist(x)) # Unlist only in case of tibble

  if(type=='factor'){
    levs <- levels(x)
    x <- as.character(x)
  }

  x[which(is.na(x))] <- ifelse(type=='numeric'|type=='integer',int_replacement,char_replacement)

  if(type=='factor'){
    newlevs <- c(levs,char_replacement)
    x <- factor(x,levels=newlevs)
  }else{
    class(x) <- type
  }

  x
}

# Replace NA in a data frame with values depending upon variable class
replace_na_df <- function(df,int_replacement=0,char_replacement='Missing'){
  lapply(df,replace_na,int_replacement=int_replacement,char_replacement=char_replacement)%>%
    bind_cols
}
