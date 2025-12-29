check_sequence <- function(dna) {
  
  dna <- toupper(gsub("[[:space:]]", "", dna))
  n <- nchar(dna)
  
  if (n == 0) return("Απορρίπτεται, η αλληλουχία ειναι κενή.")
  
  if (n %% 3 != 0) {
    return("Απορρίπτεται καθώς το μήκος της αλληλουχίας δεν είναι πολλαπλάσιο του 3.")
  }
  
  start_codon <- substr(dna, 1, 3)
  if (start_codon != "ATG") {
    return("Απορρίπτεται διότι η αλληλουχία δεν ξεκινά με κωδικόνιο έναρξης (ATG).")
  }
  
  stop_codons <- c("TAA", "TAG", "TGA")
  end_codon <- substr(dna, n - 2, n)
  if (!(end_codon %in% stop_codons)) {
    return("Απορρίπτεται καθώς η αλληλουχία δεν τελειώνει σε έγκυρο κωδικόνιο λήξης (TAA, TGA, TAG).")
  }
  
  codons <- substring(dna, seq(1, n, by=3), seq(3, n, by=3))
  
  if (length(codons) > 2) {
    internal_codons <- codons[2:(length(codons) - 1)]
    if (any(internal_codons %in% stop_codons)) {
      return("Απορρίπτεται καθώς η αλληλουχία περιέχει εσωτερικό κωδικόνιο λήξης πριν το τέλος της.")
    }
  }
  
  return("Αποδεκτή, γιατί περιέχει μια τυπική προκαρυωτική κωδικοποιούσα αλληλουχία.")
}

cat("--- Ανάλυση νουκλεοτιδικής αλυσίδας ---\n")
cat("Γράψτε την αλληλουχία.\n\n")

repeat {
  input <- readline(prompt = "DNA Sequence > ")
  
  if (input == "") {
    cat("Τέλος.\n")
    break
  }
  
  result <- check_sequence(input)
  cat("Αποτέλεσμα:", result, "\n\n")
}
