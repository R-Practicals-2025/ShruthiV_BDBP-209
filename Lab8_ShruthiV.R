# Lab8.1: Check if an integer is a palindrome
int_palin <- function(x){
  x = as.character(x)  # Convert integer to string
  rev_x <- paste(rev(strsplit(x, NULL)[[1]]), collapse = "")  # Reverse the string
  return(x == rev_x)  # Return TRUE if the string equals its reverse
}
int_palin(121)  # Should return TRUE
int_palin(123)  # Should return FALSE

# Lab8.2: Slice the string 'seemerightnow' into specific substrings
string1 <- "seemerightnow"
# 8.2.a: Extract "see" (characters 1 to 3)
substr1 <- substr(string1, 1, 3)
print(substr1)
# 8.2.b: Extract "me" (characters 4 to 5)
substr2 <- substr(string1, 4, 5)
print(substr2)
# 8.2.c: Extract "right" (characters 6 to 10)
substr3 <- substr(string1, 6, 10)
print(substr3)

# Lab8.3: Determine the fraction of G and C bases in the sequence
seq <- "ATTGCGCATAGTCCGGG"
chars <- strsplit(seq, "")[[1]]  # Split sequence into individual characters
gc_frac <- sum(chars == "G" | chars == "C") / nchar(seq)  # Count G and C and divide by total length
print(gc_frac)

# Lab8.4: Check if a DNA sequence is palindromic (equal to its reverse complement)
dna_palin <- function(x){
  x <- toupper(x)  # Ensure sequence is in uppercase
  comp <- chartr("ATGC", "TACG", x)  # Get complement: A->T, T->A, G->C, C->G
  rev_comp <- paste(rev(strsplit(comp, NULL)[[1]]), collapse = "")  # Reverse the complement
  return(x == rev_comp)  # Return TRUE if sequence equals its reverse complement
}
dna_palin("TGGATCCA")  # Should return TRUE
dna_palin("CCTGA")     # Should return FALSE

# Lab8.5: Group words in a sentence by length and print the largest and second largest groups
sentence <- "She sells hundreds of sea oysters on the sea shore."
sentence_clean <- gsub("[[:punct:]]", "", sentence)  # Remove punctuation
words <- unlist(strsplit(sentence_clean, " "))  # Split sentence into words
word_count <- nchar(words)  # Get length of each word
words_by_length <- split(words, word_count)  # Group words by their length
print("Words grouped by length:")
print(words_by_length)
# Print the group(s) of words with the largest length
print("Largest word:")
print(words_by_length[length(words_by_length)])
# Print the group(s) of words with the second largest length
print("Second largest word:")
print(words_by_length[length(words_by_length)-1])

# Lab8.6: Analysis on worldfloras data by continent
library(moments)
# 8.6.a: Load data and create separate dataframes for each continent
worldfloras <- read.table("/home/ibab/R/worldfloras.txt", header = TRUE, sep = "\t", stringsAsFactors = FALSE)
continents <- unique(worldfloras$Continent)
for(cont in continents) {
  # Create a new dataframe for each continent (df_Asia, df_Europe, etc.)
  assign(paste0("df_", cont), subset(worldfloras, Continent == cont))
}

# 8.6.b: Boxplot and summary statistics for Floral Count by continent
boxplot(Flora ~ Continent, data = worldfloras,
        main = "Floral Count Distribution by Continent",
        xlab = "Continent", ylab = "Floral Count", col = "lightblue")
# Calculate statistics: Mean, SD, Skewness, and Kurtosis for each continent's floral count
floral_stats <- lapply(split(worldfloras$Flora, worldfloras$Continent), function(x) {
  c(Mean = mean(x, na.rm = TRUE),
    SD = sd(x, na.rm = TRUE),
    Skewness = skewness(x, na.rm = TRUE),    # Right-skew: few high floral counts
    Kurtosis = kurtosis(x, na.rm = TRUE))     # High kurtosis: peaked distribution with outliers
})
print("Floral Count Statistics by Continent:")
print(floral_stats)
# Interpretation:
# Most continents show a right-skewed (positive skew) and leptokurtic distribution,
# indicating a few countries have very high floral counts.

# 8.6.c: Boxplot and histogram for Population by continent, with summary stats
boxplot(Population ~ Continent, data = worldfloras,
        main = "Population Distribution by Continent",
        xlab = "Continent", ylab = "Population", col = "lightgreen")
# Calculate statistics for population
population_stats <- lapply(split(worldfloras$Population, worldfloras$Continent), function(x) {
  c(Mean = mean(x, na.rm = TRUE),
    SD = sd(x, na.rm = TRUE),
    Skewness = skewness(x, na.rm = TRUE),   # Right-skew: a few countries have very high populations
    Kurtosis = kurtosis(x, na.rm = TRUE))    # High kurtosis: distribution is peaked with extreme values
})
print("Population statistics by Continent:")
print(population_stats)
# Interpretation:
# Population data are highly right-skewed in some continents,
# suggesting a few countries dominate in population size.
# There may be a relation with floral count patterns, but formal correlation is needed.

# Lab8.7: Process HumanBones.txt to create a dataframe with Category, Bone, and Count
lines <- readLines("/home/ibab/R/HumanBones.txt")
results <- list()  # List to store bone records
current_cat <- NA  # Current category holder

for (line in lines) {
  line <- trimws(line)  # Remove extra spaces
  if(line == "") next  # Skip empty lines
  if (!grepl("\\(", line)) {  # If line doesn't contain '(', it's a category name
    current_cat <- line
  } else {  # Otherwise, it's a bone entry
    parts <- unlist(strsplit(line, "\\("))
    bone_name <- trimws(parts[1])
    count_text <- gsub("\\).*", "", parts[2])  # Remove closing parenthesis and extra text
    count <- as.numeric(unlist(strsplit(count_text, " "))[1])  # Take the first number
    results[[length(results) + 1]] <- data.frame(Category = current_cat,
                                                 Bone = bone_name,
                                                 Count = count,
                                                 stringsAsFactors = FALSE)
  }
}
# Combine all individual bone records into one dataframe
human_bones <- do.call(rbind, results)
print(human_bones)

# Lab8.8: Create a frequency table for total bones per category and plot a bar plot
bone_freq <- aggregate(Count ~ Category, data = human_bones, FUN = function(x) sum(x))
print(bone_freq)
max_category <- bone_freq[which.max(bone_freq$Count), ]
cat("Category with maximum bones:\n")
print(max_category)
barplot(bone_freq$Count,
        names.arg = bone_freq$Category,
        main = "Total Number of Bones by Category",
        xlab = "Category",
        ylab = "Total Bones",
        col = "skyblue",
        las = 2)  # Vertical labels for clarity

# Lab8.9: Subset "Legs" category and print bone names longer than 5 letters
legs_subset <- subset(human_bones, Category == "Legs")
print(legs_subset$Bone[nchar(legs_subset$Bone) > 5])

# Lab9.10: List all bones starting with "M" and change lowercase "a" to uppercase "A"
bones_M <- human_bones$Bone[grep("^M", human_bones$Bone)]
print(gsub("a", "A", bones_M))

# Lab8.11: List bones ending with "e" and convert them to lowercase
bones_ending_with_e <- subset(human_bones, grepl("e$", Bone))
bones_ending_with_e$Bone <- tolower(bones_ending_with_e$Bone)
print(bones_ending_with_e$Bone)

# Lab8.12: List bones with two "o"s in their names
bones_with_two_o <- subset(human_bones, grepl("o.*o", Bone))
print(bones_with_two_o$Bone)


