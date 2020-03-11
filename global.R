library(datasets)

# Cairo包的PNG设备似乎无法显示中文字符，强制使用R自身的png()设备
options(shiny.usecairo = FALSE)

#指定排序, sp=指定排序的申办方(赋值的顺序也有关系), left=在左边, right=在右边
specord <- data.frame(sp = c("RD", "MST_Standards", "Overhead", "Leave"), 
                      type = c("left", "left", "right", "right"), 
                      stringsAsFactors = F)

#Top9 sponsor and "RD" "Overhead" "Leave" specified color
top  <- c("艾伯维", "Eli Lilly and Company", "Vertex", "嘉和生物", "卡德蒙", 
            "基石", "上海海和药物研究开发有限公司", "强生", "亿腾", "RD", "MST_Standards", 
            "Overhead", "Leave")
cols <- c("royalblue4", "red", "blue4", "skyblue4", "green4", 
          "darkred", "palegreen4", "deepskyblue4", "sandybrown", "mediumpurple4", "purple4",
          "dimgray","darkgray")
top_cols <- data.frame(top,cols, stringsAsFactors = F)

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
