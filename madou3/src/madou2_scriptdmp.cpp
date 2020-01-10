#include "util/TStringConversion.h"
#include "util/TBufStream.h"
#include "util/TIfstream.h"
#include "util/TOfstream.h"
#include "util/TThingyTable.h"
#include "exception/TGenericException.h"
#include <string>
#include <fstream>
#include <sstream>
#include <iostream>

using namespace std;
using namespace BlackT;

const static int op_leftbox    = 0xC0;
const static int op_rightbox   = 0xC1;
const static int op_bottombox  = 0xC2;
const static int op_br         = 0xC3;
const static int op_wait       = 0xC4;
const static int op_waitend    = 0xC4FF;
const static int op_dict       = 0xC7;
const static int op_print      = 0xD0;
const static int op_printend   = 0xD0FF;
const static int op_terminator = 0xFF;

const static int numRegion0Strings = 0x100;
const static int numRegion1Strings = 0x100;
const static int numRegion2Strings = 0x100;
const static int numRegion3Strings = 0x100;
const static int numRegion4Strings = 0x100;

const static int region_locTable_addr = 0x26ff;
const static int dictionaryTableBase = 0x1b463;

string as2bHex(int num) {
  string str = TStringConversion::intToString(num,
                  TStringConversion::baseHex).substr(2, string::npos);
  while (str.size() < 2) str = string("0") + str;
  
  return "<$" + str + ">";
}

void outputComment(std::ostream& ofs,
               string comment = "") {
  if (comment.size() > 0) {
    ofs << "//=======================================" << endl;
    ofs << "// " << comment << endl;
    ofs << "//=======================================" << endl;
    ofs << endl;
  }
}

bool opHasParam(int opcode) {
  switch (opcode) {
  case 0xC9:
  case 0xCB:
  case 0xCC:
  case 0xCE:
  case 0xCF:
  case 0xD1:
    return true;
    break;
  default:
    return false;
    break;
  }
}

void dumpSubstring(TStream& ifs, std::ostream& ofs, const TThingyTable& table,
                   int offset) {
//  std::cerr << hex << offset << endl;
  ifs.seek(offset);
  while (true) {
    TThingyTable::MatchResult result = table.matchId(ifs);
    if (result.id == -1) {
      throw TGenericException(T_SRCANDLINE,
                              "dumpSubstring(TStream&, std::ostream&)",
                              string("At offset ")
                                + TStringConversion::intToString(
                                    ifs.tell(),
                                    TStringConversion::baseHex)
                                + ": unknown character '"
                                + TStringConversion::intToString(
                                    (unsigned char)ifs.peek(),
                                    TStringConversion::baseHex)
                                + "'");
    }
    
    string resultStr = table.getEntry(result.id);
    
    if ((result.id == op_terminator)) {
      break;
    }
    
    ofs << resultStr;
  }
}

void dumpString(TStream& ifs, std::ostream& ofs, const TThingyTable& table,
              int offset, int slot,
              string comment = "") {
  ifs.seek(offset);
  
  std::ostringstream oss;
  
  if (comment.size() > 0)
    oss << "// " << comment << endl;
  
  // comment out first line of original text
  oss << "// ";
  while (true) {
    
    TThingyTable::MatchResult result = table.matchId(ifs);
    if (result.id == -1) {
      throw TGenericException(T_SRCANDLINE,
                              "dumpString(TStream&, std::ostream&)",
                              string("At offset ")
                                + TStringConversion::intToString(
                                    ifs.tell(),
                                    TStringConversion::baseHex)
                                + ": unknown character '"
                                + TStringConversion::intToString(
                                    (unsigned char)ifs.peek(),
                                    TStringConversion::baseHex)
                                + "'");
    }
    
    if ((result.id == op_dict)) {
      int dictIndex = ifs.readu8();
      int pos = ifs.tell();
      
      int offset = (dictionaryTableBase + (dictIndex * 2));
      ifs.seek(offset);
      int ptr = ifs.readu16le();
      int addr = (ptr - 0x8000) + ((ifs.tell() / 0x4000) * 0x4000);
      
      dumpSubstring(ifs, oss, table, addr);
      
      ifs.seek(pos);
      continue;
    }
    
    string resultStr = table.getEntry(result.id);
    
    if ((result.id == op_leftbox)
        || (result.id == op_rightbox)
        || (result.id == op_bottombox)) {
//      oss << endl;
      oss << endl << endl;
      oss << resultStr;
      oss << endl;
      oss << "// ";
      continue;
    }
    
    oss << resultStr;
    
    if ((result.id == op_terminator)
        || (result.id == op_waitend)
        || (result.id == op_printend)) {
//      oss << endl;
      oss << endl << endl;
      oss << resultStr;
      break;
    }
    else if (opHasParam(result.id)) {
      oss << as2bHex(ifs.readu8());
    }
    else if ((result.id == op_br)) {
      oss << endl;
      oss << "// ";
    }
/*    else if (result.id == op_clear) {
//      oss << endl;
//      oss << "// ";
      oss << endl << endl;
      oss << resultStr;
      oss << endl << endl;
      oss << "// ";
    } */
    else if ((result.id == op_wait)) {
      oss << endl << endl;
      oss << resultStr;
      oss << endl << endl;
      oss << "// ";
    }
//    else if (result.id == op_textspeed) {
//      oss << as2bHex(ifs.readu8());
//    }
  }
  
  // crude hack to remove blank comment lines.
  // doesn't work with windows linebreaks.
  {
    std::string oldstr = oss.str();
    std::string newstr;
    int i = 0;
    while (i < oldstr.size() - 5) {
      if (oldstr.substr(i, 5).compare("// \n\n") == 0) {
        i += 5;
      }
      else {
        newstr += oldstr[i];
        ++i;
      }
    }
    newstr += oldstr.substr(oldstr.size() - 5, 5);
    oss.str(newstr);
  }
  
  ofs << "#STARTMSG("
      // offset
      << TStringConversion::intToString(
          offset, TStringConversion::baseHex)
      << ", "
      // size
      << TStringConversion::intToString(
          ifs.tell() - offset, TStringConversion::baseDec)
      << ", "
      // slot num
      << TStringConversion::intToString(
          slot, TStringConversion::baseDec)
      << ")" << endl << endl;
  
  ofs << oss.str();
  
//  oss << endl;
  ofs << endl << endl;
//  ofs << "//   end pos: "
//      << TStringConversion::intToString(
//          ifs.tell(), TStringConversion::baseHex)
//      << endl;
//  ofs << "//   size: " << ifs.tell() - offset << endl;
/*      int answerTableAddr = ifs.tell();
      int answerTablePointer = (answerTableAddr % 0x4000) + 0x8000;
      int answerPointerHigh = (answerTablePointer & 0xFF00) >> 8;
      int answerPointerLow = (answerTablePointer & 0xFF);
      ofs << as2bHex(answerPointerLow) << as2bHex(answerPointerHigh) << endl; */
  ofs << endl;
  ofs << "#ENDMSG()";
  ofs << endl << endl;
}

void dumpStringSet(TStream& ifs, std::ostream& ofs, const TThingyTable& table,
               int startOffset, int slot,
               int numStrings,
               string comment = "") {
  if (comment.size() > 0) {
    ofs << "//=======================================" << endl;
    ofs << "// " << comment << endl;
    ofs << "//=======================================" << endl;
    ofs << endl;
  }
  
  ifs.seek(startOffset);
  for (int i = 0; i < numStrings; i++) {
    ofs << "// substring " << i << endl;
    dumpString(ifs, ofs, table, ifs.tell(), slot, "");
  }
}

void dumpTilemap(TStream& ifs, std::ostream& ofs, int offset, int slot,
              TThingyTable& tbl, int w, int h,
              bool isHalved = true,
              string comment = "") {
  ifs.seek(offset);
  
  std::ostringstream oss;
  
  if (comment.size() > 0)
    oss << "// " << comment << endl;
  
  // comment out first line of original text
  oss << "// ";
  for (int j = 0; j < h; j++) {
    for (int i = 0; i < w; i++) {
    
//      TThingyTable::MatchResult result = tbl.matchId(ifs);
      
      TByte next = ifs.get();
      if (!tbl.hasEntry(next)) {
        throw TGenericException(T_SRCANDLINE,
                                "dumpTilemap()",
                                string("At offset ")
                                  + TStringConversion::intToString(
                                      ifs.tell() - 1,
                                      TStringConversion::baseHex)
                                  + ": unknown character '"
                                  + TStringConversion::intToString(
                                      (unsigned char)next,
                                      TStringConversion::baseHex)
                                  + "'");
      }
      
//      string resultStr = tbl.getEntry(result.id);
      string resultStr = tbl.getEntry(next);
      oss << resultStr;
      
      if (!isHalved) ifs.get();
    }
    
    // end of line
    oss << endl;
    oss << "// ";
  }
  
//  oss << endl << endl << "[end]";
  
  ofs << "#STARTMSG("
      // offset
      << TStringConversion::intToString(
          offset, TStringConversion::baseHex)
      << ", "
      // size
      << TStringConversion::intToString(
          ifs.tell() - offset, TStringConversion::baseDec)
      << ", "
      // slot num
      << TStringConversion::intToString(
          slot, TStringConversion::baseDec)
      << ")" << endl << endl;
  
  ofs << oss.str();
  
//  oss << endl;
  ofs << endl << endl;
//  ofs << "//   end pos: "
//      << TStringConversion::intToString(
//          ifs.tell(), TStringConversion::baseHex)
//      << endl;
//  ofs << "//   size: " << ifs.tell() - offset << endl;
  ofs << endl;
  ofs << "#ENDMSG()";
  ofs << endl << endl;
}

void dumpTilemapSet(TStream& ifs, std::ostream& ofs, int startOffset, int slot,
               TThingyTable& tbl, int w, int h,
               int numTilemaps,
               bool isHalved = true,
               string comment = "") {
  if (comment.size() > 0) {
    ofs << "//=======================================" << endl;
    ofs << "// " << comment << endl;
    ofs << "//=======================================" << endl;
    ofs << endl;
  }
  
  ifs.seek(startOffset);
  for (int i = 0; i < numTilemaps; i++) {
    ofs << "// tilemap " << i << endl;
    dumpTilemap(ifs, ofs, ifs.tell(), slot, tbl, w, h, isHalved);
  }
}

void addComment(std::ostream& ofs, string comment) {
  ofs << "//===========================================================" << endl;
  ofs << "// " << comment << endl;
  ofs << "//===========================================================" << endl;
  ofs << endl;
}

const static int region_baseBank_default = 3;
const static int region_baseBank_region2 = 5;
const static int region_baseBank_region4 = 6;

int getRegionBaseBank(int regionNum) {
  switch (regionNum) {
  case 2: return region_baseBank_region2;
  case 4: return region_baseBank_region4;
  default: return region_baseBank_default;
  }
}

void dumpRegionString(TStream& ifs, std::ostream& ofs, TThingyTable& table,
                      int regionNum, int scriptNum) {
  int baseBank = getRegionBaseBank(regionNum);
  
  ifs.seek(region_locTable_addr + (regionNum * 2));
  int regionLoc = ifs.readu16le();
  
  int regionBankOffset = (regionLoc & 0xC000) >> 14;
  int regionBank = (baseBank + regionBankOffset);
  
  int regionOffset = (regionLoc & 0x3FFF);
  int regionBaseAddr = (regionBank * 0x4000) + regionOffset;
  
//  std::cerr << regionNum << " " << hex << " " << regionLoc << " " << regionBaseAddr << std::endl;
//  ifs.seek(regionBaseAddr);
//  int temp = ifs.readu16le();
//  std::cerr << hex << (temp / 2) << endl;
  
  // get offset for target script
  ifs.seek(regionBaseAddr + (scriptNum * 2));
  int scriptOffset = ifs.readu16le();
  int scriptAddr = regionBaseAddr + scriptOffset;
  
  if (scriptOffset == 0) {
    ofs << "//empty" << endl
        << "#ENDMSG()" << endl << endl;
    return;
  }
  
  // dump target script
  dumpString(ifs, ofs, table, scriptAddr, 2);
}

void dumpRegion(TStream& ifs, std::ostream& ofs, TThingyTable& table,
                int regionNum, int numScripts) {
  addComment(ofs,
    std::string("Region ") + TStringConversion::intToString(regionNum));
  ofs << "#STARTREGION(" << regionNum << ")" << endl << endl;
  for (int i = 0; i < numScripts; i++) {
    dumpRegionString(ifs, ofs, table, regionNum, i);
  }
  ofs << "#ENDREGION(" << regionNum << ")" << endl << endl;
}

int main(int argc, char* argv[]) {
  if (argc < 3) {
    cout << "Madou Monogatari II (Game Gear) script dumper" << endl;
    cout << "Usage: " << argv[0] << " [rom] [outprefix]" << endl;
    
    return 0;
  }
  
  string romName = string(argv[1]);
//  string tableName = string(argv[2]);
  string outPrefix = string(argv[2]);
  
  TBufStream ifs;
  ifs.open(romName.c_str());
  
  TThingyTable tablestd;
  tablestd.readSjis(string("table/madou2.tbl"));
  
  {
    std::ofstream ofs((outPrefix + "script.txt").c_str(),
                  ios_base::binary);
    
    dumpRegion(ifs, ofs, tablestd, 0, numRegion0Strings);
    dumpRegion(ifs, ofs, tablestd, 1, numRegion1Strings);
    dumpRegion(ifs, ofs, tablestd, 2, numRegion2Strings);
    dumpRegion(ifs, ofs, tablestd, 3, numRegion3Strings);
    dumpRegion(ifs, ofs, tablestd, 4, numRegion4Strings);
//    dumpRegionString(ifs, ofs, tablestd, 0, 0);
//    dumpRegionString(ifs, ofs, tablestd, 1, 0);
//    dumpRegionString(ifs, ofs, tablestd, 2, 0);
//    dumpRegionString(ifs, ofs, tablestd, 3, 0);
//    dumpRegionString(ifs, ofs, tablestd, 4, 0);
  }
  
  
/*  // dialogue
  {
    std::ofstream ofs((outPrefix + "dialogue.txt").c_str(),
                  ios_base::binary);
    
    addComment(ofs, "common messages");
    dumpStringSet(ifs, ofs, tablestd, 0x1BC2, 0, 1, "got item");
    dumpStringSet(ifs, ofs, tablestd, 0x1BE9, 0, 1, "used item");
    dumpStringSet(ifs, ofs, tablestd, 0x1C0F, 0, 1, "transferred item");
    dumpStringSet(ifs, ofs, tablestd, 0x1C82, 0, 1, "got ally 1?");
    dumpStringSet(ifs, ofs, tablestd, 0x247A, 0, 1, "got ally 2?");
    dumpStringSet(ifs, ofs, tablestd, 0x2647, 0, 1, "?");
    dumpStringSet(ifs, ofs, tablestd, 0x26AF, 0, 1, "learned spell");
  } */
  
  
  return 0;
} 
