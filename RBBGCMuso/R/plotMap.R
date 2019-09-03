#' plotMap

#' The function plotMap visualises data on the grid of the Carpatclim reanalysis dataset with customised colorscale by using function trimColorSet.

#' @author Roland Hollos, Erzsebet Kristof
#' @param data Numeric vector that contains data which will be plotted by plotMap function
#' @param nticks Number of colors in the colorscale (default setting is 6)
#' @param roundPrecision Number of decimal places to round off values on the colorbar
#' @param reverseColorScale If it is TRUE, the colorscale is reversed (default setting is FALSE)
#' @param colorset Name of the colorscale which is available in the list numBaseColors
#' @param center A number around which diverging colorscales will be centralised
#' @param minim Minium value of the visualised dataset
#' @param maxim Maximum value of the visualised dataset
#' 
#' @importFrom image.plot from fields
#' @importFrom colorRampPalette brewer.pal from RColorBrewer
#' @return
#' @export

trimColorSet <- function(minim, maxim, center=NULL, nticks=6, roundPrecision=NULL, reverseColorScale=FALSE, colorSet="RdYlGn") {
  
  
  if (!is.element(colorSet,rownames(brewer.pal.info))) {
    stop(sprintf("Invalid colorset, please choose from the followings:\n %s", paste(availableColors, collapse=", ")))
  }
  
  numBaseColors <- brewer.pal.info[colorSet,1]
  
  nsteps <- (maxim-minim)/nticks
  
  if(is.null(center)){
    breaks <- seq(minim,maxim,nsteps)
    if(reverseColorScale==TRUE) {
      colorbar <- rev(colorRampPalette(brewer.pal(numBaseColors,colorSet))(nticks))
    } else {
      colorbar <- colorRampPalette(brewer.pal(numBaseColors,colorSet))(nticks)
    }
    return(list(breaks=breaks,colors=colorbar))
  }
  
  breaks_full <- (function(delta){
    newRange <- c(center-delta,center+delta)
    breaks <- seq(newRange[1],newRange[2],nsteps)
    return(breaks)
  })(
    max((center - minim), (maxim-center))
  )
  
  breakConditions <- (breaks_full >= minim) & (breaks_full <= maxim)
  
  
  if(reverseColorScale==TRUE) {
    colorbar <- rev(colorRampPalette(brewer.pal(numBaseColors,colorSet))(nticks))
  } else {
    colorbar <- colorRampPalette(brewer.pal(numBaseColors,colorSet))(nticks)
  }
  return(list(breaks = breaks_full[breakConditions], colors=na.omit(colorbar[breakConditions])))
  
}

length(seq(45.8,48.5,0.1))
length(seq(16.2,22.8,0.1))

plotMap <- function(data, nticks=6, roundPrecision=NULL, reverseColorScale=FALSE,
                    colorSet="RdYlGn",center=NULL){
  

  lon <- seq(16.2,22.8,0.1)
  lat <- seq(45.8,48.5,0.1)
  
  dimlon <- length(lon)
  dimlat <- length(lat)

  index <- c(18,	19,	20,	21,	22,	23,	24,	83,	84,	85,	86,	87,	88,	89,	90,	91,	92,	94,	147,	148,	149,	150,	151,	152,	153,	154,	155,	156,	157,	158,	159,	160,
             161,	162,	163,	165,	213,	214,	215,	216,	217,	218,	219,	220,	221,	222,	223,	224,	225,	226,	227,	228,	229,	230,	231,	232,	233,	234,	278,
             279,	280,	281,	282,	283,	284,	285,	286,	287,	288,	289,	290,	291,	292,	293,	294,	295,	296,	297,	298,	299,	300,	301,	302,	303,	304,	305,
             306,	307,	308,	309,	310,	311,	312,	313,	314,	343,	344,	345,	346,	347,	348,	349,	350,	351,	352,	353,	354,	355,	356,	357,	358,	359,	360,
             361,	362,	363,	364,	365,	366,	367,	368,	369,	370,	371,	372,	373,	374,	375,	376,	377,	378,	379,	380,	381,	382,	383,	384,	385,	409,	410,
             411,	412,	413,	414,	415,	416,	417,	418,	419,	420,	421,	422,	423,	424,	425,	426,	427,	428,	429,	430,	431,	432,	433,	434,	435,	436,	437,
             438,	439,	440,	441,	442,	443,	444,	445,	446,	447,	448,	449,	450,	451,	452,	453,	474,	475,	476,	477,	478,	479,	480,	481,	482,	483,	484,
             485,	486,	487,	488,	489,	490,	491,	492,	493,	494,	495,	496,	497,	498,	499,	500,	501,	502,	503,	504,	505,	506,	507,	508,	509,	510,	511,
             512,	513,	514,	515,	516,	517,	518,	519,	520,	540,	541,	542,	543,	544,	545,	546,	547,	548,	549,	550,	551,	552,	553,	554,	555,	556,	557,
             558,	559,	560,	561,	562,	563,	564,	565,	566,	567,	568,	569,	570,	571,	572,	573,	574,	575,	576,	577,	578,	579,	580,	581,	582,	583,	584,
             585,	586,	587,	588,	606,	607,	608,	609,	610,	611,	612,	613,	614,	615,	616,	617,	618,	619,	620,	621,	622,	623,	624,	625,	626,	627,	628,
             629,	630,	631,	632,	633,	634,	635,	636,	637,	638,	639,	640,	641,	642,	643,	644,	645,	646,	647,	648,	649,	650,	651,	652,	653,	654,	655,
             656,	657,	673,	674,	675,	676,	677,	678,	679,	680,	681,	682,	683,	684,	685,	686,	687,	688,	689,	690,	691,	692,	693,	694,	695,	696,	697,
             698,	699,	700,	701,	702,	703,	704,	705,	706,	707,	708,	709,	710,	711,	712,	713,	714,	715,	716,	717,	718,	719,	720,	721,	722,	723,	724,
             738,	739,	740,	741,	742,	743,	744,	745,	746,	747,	748,	749,	750,	751,	752,	753,	754,	755,	756,	757,	758,	759,	760,	761,	762,	763,	764,
             765,	766,	767,	768,	769,	770,	771,	772,	773,	774,	775,	776,	777,	778,	779,	780,	781,	782,	783,	784,	785,	786,	787,	788,	789,	790,	791,
             792,	807,	808,	809,	810,	811,	812,	813,	814,	815,	816,	817,	818,	819,	820,	821,	822,	823,	824,	825,	826,	827,	828,	829,	830,	831,	832,
             833,	834,	835,	836,	837,	838,	839,	840,	841,	842,	843,	844,	845,	846,	847,	848,	849,	850,	851,	852,	853,	854,	855,	856,	857,	858,	859,
             875,	876,	877,	878,	879,	880,	881,	882,	883,	884,	885,	886,	887,	888,	889,	890,	891,	892,	893,	894,	895,	896,	897,	898,	899,	900,	901,
             902,	903,	904,	905,	906,	907,	908,	909,	910,	911,	912,	913,	914,	915,	916,	917,	918,	919,	920,	921,	922,	923,	924,	925,	926,	927,	942,
             943,	944,	945,	946,	947,	948,	949,	950,	951,	952,	953,	954,	955,	956,	957,	958,	959,	960,	961,	962,	963,	964,	965,	966,	967,	968,	969,
             970,	971,	972,	973,	974,	975,	976,	977,	978,	979,	980,	981,	982,	983,	984,	985,	986,	987,	988,	989,	990,	991,	992,	993,	994,	995,	1009,
             1010,	1011,	1012,	1013,	1014,	1015,	1016,	1017,	1018,	1019,	1020,	1021,	1022,	1023,	1024,	1025,	1026,	1027,	1028,	1029,	1030,	1031,	1032,	1033,	1034,	1035,	1036,
             1037,	1038,	1039,	1040,	1041,	1042,	1043,	1044,	1045,	1046,	1047,	1048,	1049,	1050,	1051,	1052,	1053,	1054,	1055,	1056,	1057,	1058,	1059,	1060,	1061,	1062,	1077,
             1078,	1079,	1080,	1081,	1082,	1083,	1084,	1085,	1086,	1087,	1088,	1089,	1090,	1091,	1092,	1093,	1094,	1095,	1096,	1097,	1098,	1099,	1100,	1101,	1102,	1103,	1104,
             1105,	1106,	1107,	1108,	1109,	1110,	1111,	1112,	1113,	1114,	1115,	1116,	1117,	1118,	1119,	1120,	1121,	1122,	1123,	1124,	1125,	1126,	1127,	1128,	1129,	1130,	1131,
             1145,	1146,	1147,	1148,	1149,	1150,	1151,	1152,	1153,	1154,	1155,	1156,	1157,	1158,	1159,	1160,	1161,	1162,	1163,	1164,	1165,	1166,	1167,	1168,	1169,	1170,	1171,
             1172,	1173,	1174,	1175,	1176,	1177,	1178,	1179,	1180,	1181,	1182,	1183,	1184,	1185,	1186,	1187,	1188,	1189,	1190,	1191,	1192,	1193,	1194,	1195,	1196,	1197,	1198,
             1212,	1213,	1214,	1215,	1216,	1217,	1218,	1219,	1220,	1221,	1222,	1223,	1224,	1225,	1226,	1227,	1228,	1229,	1230,	1231,	1232,	1233,	1234,	1235,	1236,	1237,	1238,
             1239,	1240,	1241,	1242,	1243,	1244,	1245,	1246,	1247,	1248,	1249,	1250,	1251,	1252,	1253,	1254,	1255,	1256,	1257,	1258,	1259,	1260,	1261,	1262,	1263,	1264,	1265,
             1266,	1277,	1278,	1279,	1283,	1284,	1285,	1286,	1287,	1288,	1289,	1290,	1291,	1292,	1293,	1294,	1295,	1296,	1297,	1298,	1299,	1300,	1301,	1302,	1303,	1304,	1305,
             1306,	1307,	1308,	1309,	1310,	1311,	1312,	1313,	1314,	1315,	1316,	1317,	1318,	1319,	1320,	1321,	1322,	1323,	1324,	1325,	1326,	1327,	1328,	1329,	1330,	1331,	1332,
             1333,	1334,	1350,	1351,	1352,	1353,	1354,	1355,	1367,	1368,	1369,	1370,	1371,	1372,	1373,	1374,	1375,	1376,	1377,	1378,	1379,	1380,	1381,	1382,	1383,	1384,	1385,
             1386,	1387,	1388,	1389,	1390,	1391,	1392,	1393,	1394,	1395,	1396,	1397,	1398,	1399,	1400,	1401,	1402,	1403,	1405,	1418,	1419,	1420,	1434,	1435,	1436,	1437,	1438,
             1439,	1440,	1441,	1442,	1443,	1444,	1445,	1446,	1447,	1448,	1449,	1450,	1451,	1452,	1453,	1454,	1455,	1456,	1457,	1458,	1459,	1460,	1461,	1462,	1463,	1464,	1465,
             1466,	1467,	1468,	1469,	1470,	1471,	1472,	1473,	1474,	1485,	1502,	1503,	1504,	1505,	1506,	1507,	1508,	1509,	1510,	1511,	1512,	1513,	1514,	1515,	1516,	1517,	1518,
             1519,	1520,	1521,	1522,	1523,	1524,	1525,	1526,	1527,	1528,	1529,	1530,	1531,	1532,	1533,	1534,	1535,	1536,	1537,	1538,	1539,	1540,	1541,	1575,	1576,	1577,	1578,
             1579,	1580,	1581,	1582,	1583,	1584,	1585,	1586,	1587,	1588,	1589,	1590,	1591,	1592,	1593,	1594,	1595,	1596,	1597,	1598,	1599,	1600,	1601,	1602,	1603,	1604,	1605,
             1606,	1607,	1608,	1643,	1644,	1648,	1649,	1650,	1651,	1652,	1653,	1654,	1655,	1656,	1657,	1658,	1659,	1660,	1661,	1662,	1663,	1664,	1665,	1666,	1667,	1668,	1669,
             1670,	1671,	1672,	1718,	1719,	1720,	1721,	1722,	1723,	1724,	1725,	1726,	1727,	1728,	1729,	1730,	1731,	1732,	1733,	1734,	1735,	1736,	1737,	1786,	1787,	1788,	1789,
             1790,	1791,	1792,	1793,	1794,	1795,	1796,	1797,	1803,	1854,	1855,	1856,	1857,	1858,	1860,	1861,	1862,	1863,	1864)

  
  grid_vect <- array(NA, dim=1876)
  
  
  grid_vect[index] <- data
  
  grid_array <- matrix(grid_vect, nrow=length(lon), ncol=length(lat))
   
  # breaks <- seq(floor(min(data)),ceiling(max(data)),nticks)

 # png("map.png", units="in", width=14, height=7, pointsize=14, res=100)
    colorbar <- trimColorSet(min(data),max(data),center=center, nticks=nticks,
                             roundPrecision=roundPrecision, reverseColorScale=reverseColorScale, colorSet=colorSet)
  

  windows()
  if(is.null(roundPrecision)){

    image.plot(lon, lat, grid_array2, xaxt="n", yaxt="n", ann=FALSE, col=colorbar$colors, lab.breaks=colorbar$breaks)
  }else {
    #browser()
    image.plot(lon, lat, grid_array2, xaxt="n", yaxt="n", ann=FALSE, col=colorbar$colors, lab.breaks=round(colorbar$breaks, digits=roundPrecision))
  }
  # map("world", xlim=c(lon[1],lon[length(lon)]), ylim=c(lat[1],lat[length(lat)]), add=TRUE)
  # abline(h=seq(46,48,1), v=seq(15,23,1), lty=2)
  axis(1, at=seq(17,23,1), labels=c("17°E","18°E","19°E","20°E","21°E","22°E","23°E"), cex.axis=1.2)
  axis(1, at=seq(17,23,0.5), labels=FALSE, tck=-0.01)
  axis(2, at=seq(46,48,1), labels=c("46°N","47°N","48°N"), cex.axis=1.2, las=2)
  axis(2, at=seq(46,48,0.5), labels=FALSE, tck=-0.01)
 # graphics.off()
}

testData <- runif(1104,-50,30)
plotMap(data = testData,nticks = 20,roundPrecision = 0,reverseColorScale = TRUE, colorSet = "RdBu",center = 0)
plotMap(data = testData,nticks = 6,roundPrecision = 0,reverseColorScale = TRUE, colorSet = "RdBu")
