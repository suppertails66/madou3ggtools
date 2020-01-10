#include "util/TStringConversion.h"
#include "util/TBufStream.h"
#include "util/TIfstream.h"
#include "util/TOfstream.h"
#include "util/TThingyTable.h"
#include "util/TGraphic.h"
#include "util/TPngConversion.h"
#include "util/TIniFile.h"
#include "sms/SmsPattern.h"
//#include "moldorian/MoldorianScriptReader.h"
//#include "moldorian/MoldorianLineWrapper.h"
#include "exception/TGenericException.h"
#include <string>
#include <map>
#include <vector>
#include <fstream>
#include <iostream>

using namespace std;
using namespace BlackT;
using namespace Sms;

TThingyTable table;
std::map<int, int> sizeTable;
//vector<SmsPattern> font;
map<int, SmsPattern> font;
map<int, TGraphic> fontGraphics;

struct ResultString {
  std::string str;
  int srcOffset;
  int srcSize;
  int srcSlot;
};

const static int charsPerRow = 16;
const static int baseOutputTile = 0x90;
const static int tileOrMask = 0x1800;
const static int screenTileWidth = 20;
const static int screenVisibleX = 3;

const static int op_br   = 0x90;
const static int op_wait = 0x91;
const static int op_hero = 0x92;
const static int op_op93 = 0x93;
const static int op_terminator = 0xFF;

string as2bHex(int num) {
  string str = TStringConversion::intToString(num,
                TStringConversion::baseHex).substr(2, string::npos);
  while (str.size() < 2) str = "0" + str;
  return str;
}

string getStringName(ResultString result) {
//  int bankNum = result.srcOffset / 0x4000;
  return string("string_")
    + TStringConversion::intToString(result.srcOffset,
          TStringConversion::baseHex);
}

int getStringWidth(ResultString result) {
  int width = 0;
  
  TBufStream ifs(0x10000);
  ifs.write(result.str.c_str(), result.str.size());
  ifs.clear();
  ifs.seek(0);
  
  while (!ifs.eof()) {
    TThingyTable::MatchResult result = table.matchId(ifs);
    if (result.id == -1) {
      throw TGenericException(T_SRCANDLINE,
                              "getStringWidth()",
                              "Unknown symbol at pos "
                                + TStringConversion::intToString(ifs.tell()));
    }
    
    width += sizeTable[result.id];
  }
  
  return width;
}

void composeStringGraphic(ResultString result,
                          TGraphic& dst) {
  int pixelWidth = getStringWidth(result);
  
  // for centering
/*//  std::cerr << pixelWidth << std::endl;
  int centerPixelOffset
    = ((screenTileWidth * SmsPattern::w) - pixelWidth) / 2;
//  int tileOffset = centerPixelOffset / SmsPattern::w;
  int subpixelOffset = (centerPixelOffset % SmsPattern::w);
//  if ((centerPixelOffset % SmsPattern::w) == 0) subpixelOffset = 4;
  int tileWidth = ((pixelWidth + subpixelOffset) / SmsPattern::w) + 1;
//  if ((pixelWidth % SmsPattern::w) == 0) --tileWidth; */
  
  int tileWidth = ((pixelWidth / SmsPattern::w)) + 1;
  if ((pixelWidth % SmsPattern::w) == 0) --tileWidth;
  
//  dst.resize(tileWidth * SmsPattern::w, SmsPattern::h);
  dst.resize(pixelWidth, SmsPattern::h);
  
  // "clear" with space character (index 0)
  for (int i = 0; i < tileWidth; i++) {
    font[0].toGraphic(dst, NULL,
                      i * SmsPattern::w, 0,
                      false, false, true);
  }
  
  
  TBufStream ifs(0x10000);
  ifs.write(result.str.c_str(), result.str.size());
  ifs.clear();
  ifs.seek(0);
  
//  int pos = subpixelOffset;
  int pos = 0;
  while (!ifs.eof()) {
    TThingyTable::MatchResult result = table.matchId(ifs);
    if (result.id == -1) {
      throw TGenericException(T_SRCANDLINE,
                              "composeStringGraphic()",
                              "Unknown symbol at pos "
                                + TStringConversion::intToString(ifs.tell()));
    }
    
    int charWidth = sizeTable[result.id];
    
    dst.copy(fontGraphics[result.id],
      TRect(pos, 0, sizeTable[result.id], SmsPattern::h),
      TRect(0, 0, 0, 0));
    
    pos += charWidth;
  }
}

int main(int argc, char* argv[]) {
  if (argc < 5) {
    cout << "Variable-width font string renderer" << endl;
    cout << "Usage: " << argv[0]
      << " [scriptfile] [inprefix] [thingy] [outfile]"
      << endl;
    
    return 0;
  }
  
  string scriptName = string(argv[1]);
  string inPrefix = string(argv[2]);
  string tableName = string(argv[3]);
  string outFile = string(argv[4]);
  
  table.readSjis(tableName);
  
  // read size table
/*  {
    TBufStream ifs;
    ifs.open("out/font/sizetable.bin");
    int pos = 0;
    while (!ifs.eof()) {
      sizeTable[pos++] = ifs.readu8();
    }
  } */
  
  {
    TIniFile sizeTableRaw;
    sizeTableRaw.readFile((inPrefix + "sizetable.txt"));
    for (int i = 0; i < 0x100; i++) {
      string key = as2bHex(i);
      if (sizeTableRaw.hasKey("", key)) {
        sizeTable[i] = TStringConversion::stringToInt(
          sizeTableRaw.valueOfKey("", key));
      }
      else {
        sizeTable[i] = 0;
      }
    }
  }
  
  int numChars = sizeTable.size();
  
  // font graphics
  {
    TGraphic g;
    TPngConversion::RGBAPngToGraphic((inPrefix + "font.png"), g);
    for (int i = 0; i < numChars; i++) {
      int x = (i % charsPerRow) * SmsPattern::w;
      int y = (i / charsPerRow) * SmsPattern::h;
    
      SmsPattern pattern;
      pattern.fromGrayscaleGraphic(g, x, y);
      
  //    font.push_back(pattern);
      font[i] = pattern;
      TGraphic patternGraphic(SmsPattern::w, SmsPattern::h);
      patternGraphic.copy(g,
             TRect(0, 0, 0, 0),
             TRect(x, y, SmsPattern::w, SmsPattern::h));
      fontGraphics[i] = patternGraphic;
    }
  }
  
  ResultString resultString;
  {
    TBufStream ifs;
    ifs.open((scriptName).c_str());
    
    while (!ifs.eof()) {
      TThingyTable::MatchResult r = table.matchTableEntry(ifs);
      if (r.id == -1) {
        std::cerr << "Error: unknown symbol at " << ifs.tell() << std::endl;
        return 1;
      }
      
      resultString.str += r.id;
    }
  }
  
  TGraphic g;
  composeStringGraphic(resultString, g);
  TPngConversion::graphicToRGBAPng(outFile,
                                   g);
  
//  cerr << getStringWidth(resultString) << endl;
  
  // intro text
/*  vector<TBufStream> outputTilemaps;
//  vector<SmsPattern> outputPatterns;
  TBufStream outputPatterns(0x10000);
  int outTileNum = baseOutputTile;
  {
    TBufStream ifs;
//    ifs.open((inPrefix + "script.txt").c_str());
    ifs.open((inPrefix + "intro.txt").c_str());
    
    MoldorianScriptReader::ResultCollection results;
    MoldorianScriptReader(ifs, results, table)();
    
    for (unsigned int i = 0; i < results.size(); i++) {
//      std::cerr << "string " << i << std::endl;
//      cout << getStringWidth(results[i]) << endl;
      TGraphic stringGraphic;
      composeStringGraphic(results[i], stringGraphic);
//      TPngConversion::graphicToRGBAPng("test_" + TStringConversion::intToString(i) + ".png",
//                                       stringGraphic);
      
      TBufStream tilemapOfs(0x10000);
      int tileW = stringGraphic.w() / SmsPattern::w;
      // number of tiles
      tilemapOfs.writeu8(tileW);
      // centering offset
      int centerOffset = ((screenTileWidth - tileW) / 2);
      tilemapOfs.writeu8(centerOffset + screenVisibleX);
      for (int j = 0; j < tileW; j++) {
        SmsPattern pattern;
        pattern.fromGrayscaleGraphic(stringGraphic, j * SmsPattern::w, 0);
//        outputPatterns.push_back(pattern);
        pattern.write(outputPatterns);
        
        int tileId = outTileNum++ | tileOrMask;
        tilemapOfs.writeu16le(tileId);
      }
      
      outputTilemaps.push_back(tilemapOfs);
    }
    
  }
  
  outputPatterns.save((outPrefix + "/intro/grp.bin").c_str());
  
  TBufStream outputTilemapTable(0x10000);
  outputTilemapTable.writeu8(outputTilemaps.size());
//  {
//    int offset = (outputTilemaps.size() * 2);
//    for (unsigned int i = 0; i < outputTilemaps.size(); i++) {
//      outputTilemapTable.writeu16le(offset);
//      offset += outputTilemaps[i].size();
//    }
//  }
  
  for (unsigned int i = 0; i < outputTilemaps.size(); i++) {
    TBufStream& ofs = outputTilemaps[i];
    ofs.seek(0);
    outputTilemapTable.writeFrom(ofs, ofs.size());
  }
  
  outputTilemapTable.save((outPrefix + "/intro/tilemaps.bin").c_str()); */
  
  return 0;
}

