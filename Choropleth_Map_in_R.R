library(maps)      # for map data
library(rgdal)     # for reading a shape file
library(maptools)  # for reading a shape file
library(plyr)      # for using arrange() function (see Step 5-6)
library(ggplot2)

##################################################################          ######
# 1. Plot a World Map                                                       ######
# Get Map Data for WORLD
World_Map <- map_data( 'world' )

# Plot World Map
ggplot( World_Map, aes( x = long, y = lat, group = group ) ) + 
  geom_polygon( fill = 'gray',     # 區域顏色
                colour = 'black'   # 邊界顏色
                ) + 
  labs( title = 'World Map',       # 圖標題
        x = 'Longitude', y = 'Latitude' )


##################################################################          ######
# 2. Plot East Asia Map                                                     ######
# Get Map Data for East Asia
East_Asia <- map_data( 'world', region = c('North Korea', 'South Korea', 'Japan', 'China', 'Taiwan') )

# Plot East Asia Map
ggplot( East_Asia, aes( x = long, y = lat, group = group, fill = region ) ) + 
  geom_polygon( colour = 'black'    # 邊界顏色 
                ) + 
  scale_fill_brewer( palette = 'Set2' ) + 
  labs( title = 'East Asia',
        x = 'Longitude', y = 'Latitude' )


##################################################################          ######
# 3. Plot Taiwan Map                                                        ######
# Get Map Data for Taiwan
Taiwan_Map <- map_data( 'world', region = c('Taiwan') )

# Plot Taiwan Map
ggplot( Taiwan_Map, aes( x = long, y = lat, group = group ) ) + 
  geom_polygon( fill = 'white',     # 區域顏色
                colour = 'black'    # 邊界顏色
                ) + 
  coord_map( 'polyconic' ) +        # 設定座標系統
  labs( title = 'Taiwan Map',
        x = 'Longitude', y = 'Latitude' ) 


##################################################################          ######
# 4. Creat a Map from a Shapefile                                           ######
# Example 1                                                                 ######
# 資料來源：政府資料開放平臺(DATA.GOV.TW)  空氣品質範圍圖                   ######
#           https://data.nat.gov.tw/dataset/6380
#
# Step 4-1.1 Read a shape file                                              ######
# 方法一：
shapefile <- readOGR( dsn = ".", 
                      layer ="空氣品質區範圍圖_10707")
# 註：dsn = 資料夾路徑；layer = 檔案名稱( 不需 .shp )

#方法二：
#shp_path <- './空氣品質區範圍圖_10707.shp'
#shpae_file <- readShapePoly( shp_path )
# 方法二會出錯：
# readShapePoly is deprecated; use rgdal::readOGR or sf::st_read !!


# Step 4-1.2 Conver a shape file to a regular data frame                    ######
# 將 shape file 轉成 data frame 的資料型態
shapefile_df <- fortify( shapefile )
head( shapefile_df )


# Step 4-1.3 Plot a Map                                                     #####
ggplot( shapefile_df, aes( x = long, y = lat, group = group )) + 
  geom_path( ) + 
  geom_polygon( fill = 'white',     # 區域顏色
                colour = 'black'    # 邊界顏色  
              ) + 
  labs( title = 'Example 1: Taiwan Map from DATA.GOV.TW',  # 圖標題
        x = 'Longitude', y = 'Latitude' ) 


# ------------------------------------------------------                    ######
# ------------------------------------------------------                    ######
# Example 2                                                                 ######
# 資料來源：DIVA-GIS ( http://www.diva-gis.org/gdata )                      ######
#
# Step 4-2.1 Read a shape file                                              ######
shapefile_2 <- readOGR( dsn = '.',
                        layer = 'TWN_adm2' )


# Step 4-2.2 Conver a shape file to a regular data frame                    ######
# 將 shape file 轉成 data frame 的資料型態
shapefile_2_df <- fortify( shapefile_2 )


# Step 4-2.3 Plot a Map                                                     #####
ggplot( shapefile_2_df, aes( x = long, y = lat, group = group ) ) + 
  geom_path( ) + 
  labs( title = 'Example 2: Taiwan Map from DIVA-GIS',
        x = 'Longitude', y = 'Latitude' ) 


##################################################################          ######
# 5. Creat a Choropleth Map                                                 ######
# (1) 數據資料來源：                                                        ######
#     (i) 空氣品質監測站基本資料 from 政府資料開放平臺 
#         網頁：https://data.gov.tw/dataset/6075
#     (ii) 空氣品質監測月值 from 環保署環境資源資料庫(類別：大氣)
#          網頁：https://erdb.epa.gov.tw/ERDBIndex.aspx
#
#  註：因資料量不大及串接欄位不多，故先用 Excel 結合上面兩種資訊的資料，
#      再載入 R 作為繪圖用的數據資料 。
#
# (2) shape file 資料來源：
#     DIVA-GIS( 網頁：http://www.diva-gis.org/gdata )  
#                                                                           ######
# Step 5-1 Read a value data                                                ######
# 載入資料時，避免產生中文亂碼問題 
Sys.setlocale( 'LC_ALL', 'en_US.UTF-8' )  

# 數據資料的檔案路徑
Value_Data_Path <- './空氣品質月監測值201811.txt'

# 載入資料
Value_Data <- read.table( Value_Data_Path, header = TRUE, sep = ',', fileEncoding = 'UTF-8' )


# Step 5-2 Create a new column 'TAIWAN_COUNTY' of value data                ######
# 查看 COUNTY 欄位中的數據種類
levels( Value_Data$COUNTY )

# 新增 region 欄位
Value_Data$region <- c( 'Taiwan' ) 

# 定義 Mapping Function : group -> TAIWAN_COUNTY
get_TAIWAN_COUNTY_1 <- function( group ) {
  if ( group == c('臺北市') ) {
    return( 'Taipei' )
  } else if ( group == c('新北市') ) {
    return( 'New Taipei City' )
  } else if ( group == c('桃園市') ) {
    return( 'Taoyuan' )
  } else if ( group == c('臺中市') ) {
    return( 'Taichung' )
  } else if ( group == c('高雄市') ) {
    return( 'Kaoshiung' )
  } else if ( group == c('新竹市') | group == c('新竹縣') ) {
    return( 'Hsinchu' )
  } else if ( group == c('苗栗縣') ) {
    return( 'Miaoli' )
  } else if ( group == c('南投縣') ) {
    return( 'Nantou' )
  } else if ( group == c('雲林縣') ) {
    return( 'Yunlin' )
  } else if ( group == c('彰化縣') ) {
    return( 'Changhua' )
  } else if ( group == c('嘉義市') | group == c('嘉義縣') ) {
    return( 'Chiayi' )
  } else if ( group == c('臺南市') ) {
    return( 'Tainan' )
  } else if ( group == c('屏東縣') ) {
    return( 'Pingtung' )
  } else if ( group == c('臺東縣') ) {
    return( 'Taitung' )
  } else if ( group == c('花蓮縣') ) {
    return( 'Hualien' )
  } else if ( group == c('宜蘭縣') ) {
    return( 'Yilan' )
  } else if ( group == c('基隆市') ) {
    return( 'Keelung' )
  } else if ( group == c('澎湖縣') ) {
    return( 'Penghu' )
  } else if ( group == c('金門縣') | group == c('連江縣') ) {
    return( 'Others' )
  } else {
    return( 'NaN' )
  }
}

# Apply the above Mapping Function
Value_Data$TAIWAN_COUNTY <- sapply( Value_Data$COUNTY, 
                                    FUN = get_TAIWAN_COUNTY_1 )

# 檢查新增後的資料
head( Value_Data )


# Step 5-3 Read a shape file                                                ######
# Read a shape file from DIVA-GIS( http://www.diva-gis.org/gdata )
shapefile_2 <- readOGR( dsn = '.', 
                        layer = 'TWN_adm2' )

# Conver a shape file to a regular data frame
shapefile_2_df <- fortify( shapefile_2 )


# Step 5-4 Create a new column 'TAIWAN_COUNTY' of shape file                ######
# 查看 group 欄位中的資料類別
levels( shapefile_2_df$group )

# 定義 Mapping Function : group -> TAIWAN_COUNTY
get_TAIWAN_COUNTY_2 <- function( group ) {
  if ( group == c('0.1') | group == c('0.2') |
       group == c('8.1') | group == c('8.2') ) {
    return( 'Kaoshiung' )
  } else if ( group == c('1.1') ) {
    return( 'Dongsha Islands' )
  } else if ( group == c('2.1') ) {
    return( 'Taipei' )
  } else if ( group == c('18.1') | group == c('18.2') ) {
    return( 'New Taipei City' )
  } else if ( group == c('20.1') ) {
    return( 'Taoyuan' )
  } else if ( group == c('3.1') | group == c('21.1') | 
              group == c('21.2') | group == c('21.3') ) {
    return( 'Yunlin' )
  } else if ( group == c('3.2') | group == c('3.3') | 
              group == c('3.4') | group == c('15.1') ) {
    return( 'Changhua' )
  } else if ( group == c('4.1') | group == c('4.2') | 
              group == c('4.3') | group == c('4.4') | 
              group == c('4.5') ) {
    return( 'Chiayi' )
  } else if ( group == c('5.1') | group == c('5.2') ) {
    return( 'Hsinchu' )
  } else if ( group == c('6.1') | group == c('11.1') ) {
    return( 'Nantou' )
  } else if ( group == c('6.2') | group == c('6.3') ) {
    return( 'Hualien' )
  } else if ( group == c('19.1') | group == c('19.2') | 
              group == c('19.3') | group == c('19.4') |
              group == c('19.5') | group == c('19.6') |
              group == c('19.7') ) {
    return( 'Taitung' )
  } else if ( group == c('7.1') | group == c('7.2') ) {
    return( 'Yilan' )
  } else if ( group == c('9.1') | group == c('9.2') |
              group == c('18.3') | group == c('18.4') ) {
    return( 'Keelung' )
  } else if ( group == c('10.1') | group == c('14.1') | 
              group == c('15.2') ) {
    return( 'Taichung' )
  } else if ( group == c('13.1') | group == c('13.2') ) {
    return( 'Pingtung' )
  } else if ( group == c('16.1') | group == c('17.1') ) {
    return( 'Tainan' )
  } else if ( group == c('12.1') | group == c('12.2') |
              group == c('12.3') | group == c('12.4') | 
              group == c('12.5') | group == c('12.6') |
              group == c('12.7') | group == c('12.8') |
              group == c('12.9') | group == c('12.10') | 
              group == c('12.11') | group == c('12.12') |
              group == c('12.13') | group == c('12.14') | 
              group == c('12.15') | group == c('12.16') |
              group == c('12.17') | group == c('12.18') |
              group == c('12.19') | group == c('12.20') |
              group == c('12.21') | group == c('12.22') |
              group == c('12.23') | group == c('12.24') |
              group == c('12.25') | group == c('12.26') |
              group == c('12.27') | group == c('12.28') |
              group == c('12.29') | group == c('12.30') |
              group == c('12.31') | group == c('12.32') |
              group == c('12.33') | group == c('12.34') |
              group == c('12.35') | group == c('12.36') | 
              group == c('12.37') | group == c('12.38') | 
              group == c('12.39') | group == c('12.40') | 
              group == c('12.41') | group == c('12.42') | 
              group == c('12.43') | group == c('12.44') | 
              group == c('12.45') | group == c('12.46') | 
              group == c('12.47') | group == c('12.48') |
              group == c('12.49') | group == c('12.50') |
              group == c('12.51') | group == c('12.52') |
              group == c('12.53') | group == c('12.54') | 
              group == c('12.55') | group == c('12,56') | 
              group == c('12.57') | group == c('12.58') |
              group == c('12.59') | group == c('12.60') |
              group == c('12.61') ) {
    return( 'Penghu' )
  }
  else {
    return("Others")
  }
}

# Apply the above Mapping Function
shapefile_2_df$TAIWAN_COUNTY <- sapply( shapefile_2_df$group, 
                                        FUN = get_TAIWAN_COUNTY_2 )

# 檢查新增後的資料
head( shapefile_2_df )


# Step 5-5 Merge the data sets together by new column                       ######
# 以 'TAIWAN_COUNTY' 欄位為基準，
# 合併上面 shape file 及 value data 兩個資料集
Mapping_Data <- merge( shapefile_2_df, Value_Data, 
                       by = 'TAIWAN_COUNTY' )
head( Mapping_Data )


# Step 5-6 Sort by group, then order                                        ######
Mapping_Data <- arrange( Mapping_Data, group.x, order )
head( Mapping_Data )


# Step 5-7 Plot a choropleth map with the default color scale               ######
# Use the default color scale, which goes from dark to light blue. 
ggplot( Mapping_Data, 
        aes( x = long.x, y = lat.x, 
             group = group.x, fill = PM.2.5 ) ) +
geom_polygon( colour = 'white' ) +
coord_map( 'polyconic' ) +    # 設定座標系統
labs( title = 'PM 2.5 in Taiwan (Nov. 2018)',
      x = 'Longitude', y = 'Latitude' )


# Step 5-8 Adjust color of choropleth map                                   ######
# If we want to show how the values diverge from some middle value,
# we can use "scale_fill_gradient2()" function.
ggplot( Mapping_Data, 
          aes( x = long.x, y = lat.x, 
               group = group.x, fill = PM.2.5 ) ) +
  geom_polygon( colour = 'grey' ) +
  scale_fill_gradient2( low = '#559999', mid = 'grey90', high = '#BB650B',
                        midpoint = median( Mapping_Data$PM.2.5 )) +
  coord_map( 'polyconic' ) +     # 設定座標系統
  labs( title = 'PM 2.5 in Taiwan (Nov. 2018)',
        x = 'Longitude', y = 'Latitude' )

