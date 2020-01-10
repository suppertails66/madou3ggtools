#include "util/TStringConversion.h"
#include "util/TBufStream.h"
#include "util/TIfstream.h"
#include "util/TOfstream.h"
#include "util/TThingyTable.h"
#include "madou2/Madou2ScriptReader.h"
#include "madou2/Madou2LineWrapper.h"
#include "exception/TGenericException.h"
#include <string>
#include <map>
#include <fstream>
#include <iostream>

using namespace std;
using namespace BlackT;
using namespace Sms;

TThingyTable table;

const static int hashMask = 0x0FFF;

const static int controlOpsStart = 0xC0;
const static int controlOpsEnd   = 0x100;

const static int code_space   = 0x00;
//const static int code_clear   = 0xFD;
const static int code_wait    = 0xC4;
const static int code_br      = 0xC3;
const static int code_end     = 0xFF;

// added for translation
const static int code_tilebr  = 0x1F;

string getStringName(Madou2ScriptReader::ResultString result) {
//  int bankNum = result.srcOffset / 0x4000;
  return string("string_")
    + TStringConversion::intToString(result.srcOffset,
          TStringConversion::baseHex);
}

void exportRawResults(Madou2ScriptReader::ResultCollection& results,
                      std::string filename) {
  TBufStream ofs(0x10000);
  for (int i = 0; i < results.size(); i++) {
    ofs.write(results[i].str.c_str(), results[i].str.size());
  }
  ofs.save((filename).c_str());
}

void exportRawResults(TStream& ifs,
                      std::string filename) {
  Madou2ScriptReader::ResultCollection results;
  Madou2ScriptReader(ifs, results, table)();
  exportRawResults(results, filename);
}

void exportTabledResults(TStream& ifs,
                         std::string binFilename,
                         Madou2ScriptReader::ResultCollection& results,
                         TBufStream& ofs) {
  int offset = 0;
  for (int i = 0; i < results.size(); i++) {
    ofs.writeu16le(offset + (results.size() * 2));
    offset += results[i].str.size();
  }
  
  for (int i = 0; i < results.size(); i++) {
    ofs.write(results[i].str.c_str(), results[i].str.size());
  }
  
  ofs.save((binFilename).c_str());
}

void exportTabledResults(TStream& ifs,
                         std::string binFilename) {
  Madou2ScriptReader::ResultCollection results;
  Madou2ScriptReader(ifs, results, table)();
  
//  std::ofstream incofs(incFilename.c_str());
  TBufStream ofs(0x10000);
  exportTabledResults(ifs, binFilename, results, ofs);
}

void exportSizeTabledResults(TStream& ifs,
                         std::string binFilename) {
  Madou2ScriptReader::ResultCollection results;
  Madou2ScriptReader(ifs, results, table)();
  
//  std::ofstream incofs(incFilename.c_str());
  TBufStream ofs(0x10000);
  ofs.writeu8(results.size());
  exportTabledResults(ifs, binFilename, results, ofs);
}

void generateHashTable(string infile, string outPrefix, string outName) {
  TBufStream ifs;
//    ifs.open((inPrefix + "script.txt").c_str());
//  ifs.open((outPrefix + "script_wrapped.txt").c_str());
  ifs.open(infile.c_str());
  
  Madou2ScriptReader::ResultCollection results;
  Madou2ScriptReader(ifs, results, table)();
  
//    TBufStream ofs(0x20000);
//    for (unsigned int i = 0; i < results.size(); i++) {
//      ofs.write(results[i].str.c_str(), results[i].str.size());
//    }
//    ofs.save((outPrefix + "script.bin").c_str());
  
  // create:
  // * an individual .bin file for each compiled string
  // * a .inc containing, for each string, one superfree section with an
  //   incbin that includes the corresponding string's .bin
  // * a .inc containing the hash bucket arrays for the remapped strings.
  //   table keys are (orig_pointer & 0x1FFF).
  //   the generated bucket sets go in a single superfree section.
  //   each bucket set is an array of the following structure (terminate
  //   arrays with FF so we can detect missed entries):
  //       struct Bucket {
  //       u8 origBank
  //       u16 origPointer  // respects original slotting!
  //       u8 newBank
  //       u16 newPointer
  //     }
  // * a .inc containing the bucket array start pointers (keys are 16-bit
  //   and range from 0x0000-0x1FFF, so this gets its own bank)
  
  std::ofstream strIncOfs(
    (outPrefix + "strings" + outName + ".inc").c_str());
  std::map<int, Madou2ScriptReader::ResultCollection>
    mappedStringBuckets;
  for (unsigned int i = 0; i < results.size(); i++) {
    std::string stringName = getStringName(results[i]) + outName;
    
    // write string to file
    TBufStream ofs(0x10000);
    ofs.write(results[i].str.c_str(), results[i].str.size());
    ofs.save((outPrefix + "strings/" + stringName + ".bin").c_str());
    
    // add string binary to generated includes
    strIncOfs << ".slot 2" << endl;
    strIncOfs << ".section \"string include " << outName << " "
      << i << "\" superfree"
      << endl;
    strIncOfs << "  " << stringName << ":" << endl;
    strIncOfs << "    " << ".incbin \""
      << outPrefix << "strings/" << stringName << ".bin"
      << "\"" << endl;
    strIncOfs << ".ends" << endl;
    
    // add to map
    mappedStringBuckets[results[i].srcOffset & hashMask]
      .push_back(results[i]);
  }
  
  // generate bucket arrays
  std::ofstream stringHashOfs(
    (outPrefix + "string_bucketarrays" + outName + ".inc").c_str());
  stringHashOfs << ".include \""
    << outPrefix + "strings" + outName + ".inc\""
    << endl;
  stringHashOfs << ".section \"string hash buckets " << outName
    << "\" superfree" << endl;
  stringHashOfs << "  stringHashBuckets" + outName + ":" << endl;
  for (std::map<int, Madou2ScriptReader::ResultCollection>::iterator it
         = mappedStringBuckets.begin();
       it != mappedStringBuckets.end();
       ++it) {
    int key = it->first;
    Madou2ScriptReader::ResultCollection& results = it->second;
    
    stringHashOfs << "  hashBucketArray_"
      << outName
      << TStringConversion::intToString(key,
            TStringConversion::baseHex)
      << ":" << endl;
    
    for (unsigned int i = 0; i < results.size(); i++) {
      Madou2ScriptReader::ResultString result = results[i];
      string stringName = getStringName(result) + outName;
      
      // original bank
      stringHashOfs << "    .db " << result.srcOffset / 0x4000 << endl;
      // original pointer (respecting slotting)
      stringHashOfs << "    .dw "
        << (result.srcOffset & 0x3FFF) + (0x4000 * result.srcSlot)
        << endl;
      // new bank
      stringHashOfs << "    .db :" << stringName << endl;
      // new pointer
      stringHashOfs << "    .dw " << stringName << endl;
    }
    
    // array terminator
    stringHashOfs << "  .db $FF " << endl;
  }
  stringHashOfs << ".ends" << endl;
  
  // generate bucket array hash table
  std::ofstream bucketHashOfs(
    (outPrefix + "string_bucket_hashtable" + outName + ".inc").c_str());
  bucketHashOfs << ".include \""
    << outPrefix + "string_bucketarrays" + outName + ".inc\""
    << endl;
  bucketHashOfs
    << ".section \"bucket array hash table " << outName
      << "\" size $4000 align $4000 superfree"
    << endl;
  bucketHashOfs << "  bucketArrayHashTable" << outName << ":" << endl;
  for (int i = 0; i < hashMask; i++) {
    std::map<int, Madou2ScriptReader::ResultCollection>::iterator findIt
      = mappedStringBuckets.find(i);
    if (findIt != mappedStringBuckets.end()) {
      int key = findIt->first;
      // bucket bank
      bucketHashOfs << "    .db :hashBucketArray_" + outName
        << TStringConversion::intToString(key,
              TStringConversion::baseHex)
        << endl;
      // bucket pointer
      bucketHashOfs << "    .dw hashBucketArray_" + outName
        << TStringConversion::intToString(key,
              TStringConversion::baseHex)
        << endl;
      // reserved
      bucketHashOfs << "    .db $FF"
        << endl;
    }
    else {
      // no array
      bucketHashOfs << "    .db $FF,$FF,$FF,$FF" << endl;
    }
  }
  bucketHashOfs << ".ends" << endl;
}

int main(int argc, char* argv[]) {
  if (argc < 4) {
    cout << "Madou Monogatari II (Game Gear) script builder" << endl;
    cout << "Usage: " << argv[0] << " [inprefix] [thingy] [outprefix]"
      << endl;
    
    return 0;
  }
  
  string inPrefix = string(argv[1]);
  string tableName = string(argv[2]);
  string outPrefix = string(argv[3]);
  
  table.readSjis(tableName);
  
  // wrap script
  {
    // read size table
    Madou2LineWrapper::CharSizeTable sizeTable;
    {
      TBufStream ifs;
      ifs.open("out/font/sizetable.bin");
      int pos = 0;
      while (!ifs.eof()) {
        sizeTable[pos++] = ifs.readu8();
      }
    }
    
    {
      TBufStream ifs;
      ifs.open((inPrefix + "script.txt").c_str());
//      ifs.open(("out/script/dialogue_all.txt"));
      
      TLineWrapper::ResultCollection results;
      Madou2LineWrapper(ifs, results, table, sizeTable)();
      
      if (results.size() > 0) {
        TOfstream ofs((outPrefix + "script_wrapped.txt").c_str());
        ofs.write(results[0].str.c_str(), results[0].str.size());
      }
    }
  }
  
//  generateHashTable((outPrefix + "script_wrapped.txt"),
//                    outPrefix,
//                    "script");

  {
    TBufStream ifs;
    ifs.open((outPrefix + "script_wrapped.txt").c_str());
    
    exportTabledResults(ifs, outPrefix + "region0.bin");
    exportTabledResults(ifs, outPrefix + "region1.bin");
    exportTabledResults(ifs, outPrefix + "region2a.bin");
    exportTabledResults(ifs, outPrefix + "region2b.bin");
    exportTabledResults(ifs, outPrefix + "region3.bin");
    exportTabledResults(ifs, outPrefix + "region4.bin");
  }
  
  // tilemaps/new
/*  {
    TBufStream ifs;
    ifs.open((inPrefix + "tilemaps.txt").c_str());
    
    exportRawResults(ifs, outPrefix + "roulette_right.bin");
    exportRawResults(ifs, outPrefix + "roulette_wrong.bin");
    exportRawResults(ifs, outPrefix + "roulette_timeup.bin");
    exportRawResults(ifs, outPrefix + "roulette_perfect.bin");
    exportRawResults(ifs, outPrefix + "roulette_blank.bin");
    
    exportRawResults(ifs, outPrefix + "mainmenu_help.bin");
    
    exportSizeTabledResults(ifs, outPrefix + "credits.bin");
  } */
  
  return 0;
}

