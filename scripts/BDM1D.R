get_k_values <- function(alphabet_size) {
  
  name <- paste0("K", as.character(alphabet_size))
  path <- paste0("data/", name, ".rds")
  
  k_data <- as.data.frame(readRDS(path))
  k_data$S <- NULL
  
  return(k_data)
}


maxKnownKs <- read.csv("data/maxKnownKs.csv")
# Erase useless column
maxKnownKs$X <- NULL


count_symbols <- function(x) {
  return(length(table(strsplit(x, NULL))))
}

number_to_binary <- function(x, no_bits) {
  
  binary_vector = rev(as.numeric(intToBits(x)))
  
  if (missing(no_bits)) {
    return(binary_vector)
  } else {
    binary_vector[-(1:(length(binary_vector) - no_bits))]
  }
}

get_bits_in_string <-function(x) {
  
  x <- utf8ToInt(x)
  
  bit_list <- lapply(x, number2binary, no_bits = 8)
  bit_list2 <- lapply(bit_list, paste0, collapse = "")
  
  bits_in_string <- paste0(bit_list2, collapse = "")
  
  return(bits_in_string)
}

split_string <- function(x, block_size, offset) {
  
  if (block_size > nchar(x)) {
    return(x)
  }
  
  if (offset > block_size) {
    return ("ERROR: Offset cannot be greater than block size.")
  }
  
  subs <- character()
  start_indices <- seq(1, nchar(x), offset)
  
  for (i in start_indices){
    first <- i
    last <- -1
    
    if (last > nchar(x)) {
      last <- nchar(x) - 1
    } else {
      last <- i + block_size - 1
    }
    
    sub <- substr(x, first, last)
    subs <- append(subs, sub)
    
    last_step = FALSE
    if (nchar(sub) == block_size && last == nchar(x)) {
      last_step = TRUE
    }
    if (last_step) {
      break
    }
  }
  return(subs)
}

# Receives the already splitted vector of input strings
get_bdm <- function (strings_vector, k_values, base) {
  
  string_counts <- as.data.frame(table(strings_vector))
  string_counts$strings_vector <- as.character(string_counts$strings_vector)
  string_counts["ks"] <- k_values[string_counts$strings_vector, ]
  
  na_indices <- as.integer(which(is.na(string_counts$ks)))
  na_strings <- as.vector(string_counts$strings_vector[na_indices])
  na_lengths <- unlist(lapply(na_strings, nchar))
  
  # More complex (+1) than the highest known values
  naKs <- maxKnownKs[, paste0("K.", toString(base))] + 1
  
  string_counts[is.na(string_counts)] <- naKs
  bdm <- sum(log2(string_counts$Freq)) + sum(string_counts$ks)
  
  return(bdm)
}

normalize_string <- function(x) {
  
  splitted <- strsplit(x, "")
  elements <- lapply(splitted, unique)
  
  if (any(vapply(elements, length, 0) > 10)) {
    stop("Too many symbols (more than 10)")
  }
  
  exchanged <- mapply(function(a, b) seq(0, length.out = length(a))[match(b, a)],
                      elements, splitted, SIMPLIFY = FALSE)
  
  return(vapply(exchanged, paste, "", collapse = ""))
}

# Get BDM value of a given string 'x' after normalizing it
bdm1D <- function(x, block_size, offset, base, k_values){
  
  splitted_string <- split_string(x, block_size, offset)
  normalized_string <- unlist(lapply(splitted_string, normalize_string))
  
  bdm <- get_bdm(strings_vector = normalized_string, k_values = k_values, base = 2 )
  
  return(bdm)
}