# install.packages("tesseract")
library(tesseract)


# First get the PDFs into images
# https://cran.r-project.org/web/packages/tesseract/vignettes/intro.html


pngfile <- pdftools::pdf_convert('inputs/Sample dictionary pages.pdf', dpi = 600)

pngfile[1]

tesseract_info()

tesseract_download("jpn")
japanese <- tesseract("jpn")

text <- ocr("asdf.png", engine = japanese)

cat(text)


tesseract_download("jpn_vert")
japanese_vertical <- tesseract("jpn_vert")


text <- ocr("top_row_of_first_page.png", engine = japanese_vertical)


english_and_japanese <- tesseract("eng+jpn_vert")

# Try pre-processing a bit more
library(magick)
image_read(pngfile[1]) %>% 
  # image_info()
  image_crop("1700x1050+300+270") %>% 
  image_convert(colorspace = 'gray') %>%
  image_trim() %>% 
  # image_scale("x2000") %>% 
  image_background("white", flatten = TRUE) %>% 
  image_noise() %>%
  image_enhance() %>% 
  image_normalize() %>% 
  image_contrast(sharpen = 1) %>% 
  ocr(engine = japanese_vertical) %>% 
  cat()


text <- ocr("top_row_of_first_page.png", engine = japanese_vertical)


all <- tesseract_params()
tesseract_params('psm')
tesseract_params('use_new_state_cost')
tesseract_params('use_new_state_cost')