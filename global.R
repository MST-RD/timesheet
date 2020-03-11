library(datasets)

# Cairo包的PNG设备似乎无法显示中文字符，强制使用R自身的png()设备
options(shiny.usecairo = FALSE)

#Top9 sponsor and "not bill hour" specified color
top10  <- c("艾伯维", "Eli Lilly and Company", "Vertex", "嘉和生物", "卡德蒙", 
            "基石", "上海海和药物研究开发有限公司", "强生", "not Bill hour", "亿腾")
cols <- c("royalblue4", "red", "blue4", "skyblue4", "green4", 
          "darkred", "palegreen4", "deepskyblue4", "dimgray", "sandybrown")
top10_cols <- data.frame(top10,cols, stringsAsFactors = F)

# 请忽略以下代码，它只是为了解决ShinyApps上没有中文字体的问题
font_home <- function(path = '') file.path('~', '.fonts', path)
if (Sys.info()[['sysname']] == 'Linux' &&
    system('locate wqy-zenhei.ttc') != 0 &&
    !file.exists(font_home('wqy-zenhei.ttc'))) {
  if (!file.exists('wqy-zenhei.ttc'))
    curl::curl_download(
      'https://github.com/rstudio/shiny-examples/releases/download/v0.10.1/wqy-zenhei.ttc',
      'wqy-zenhei.ttc'
    )
  dir.create(font_home())
  file.copy('wqy-zenhei.ttc', font_home())
  system2('fc-cache', paste('-f', font_home()))
}
rm(font_home)


if (.Platform$OS.type == "windows") {
  if (!grepl("Chinese", Sys.getlocale())) {
    warning(
      "You probably want Chinese locale on Windows for this app",
      "to render correctly. See ",
      "https://github.com/rstudio/shiny/issues/1053#issuecomment-167011937"
    )
  }
}