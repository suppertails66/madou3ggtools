#include "lastbible/LastBibleLineWrapper.h"
#include "util/TParse.h"
#include "util/TStringConversion.h"
#include "exception/TGenericException.h"
#include <iostream>

using namespace BlackT;

namespace Sms {

const static int controlOpsStart = 0xEF;
const static int controlOpsEnd   = 0x100;

const static int code_space   = 0x20;

const static int code_stringbuf = 0xEF;
const static int code_charname  = 0xF0;
const static int code_wait      = 0xF1;
const static int code_br        = 0xF2;
const static int code_textspeed = 0xF3;
const static int code_clear     = 0xF4;
const static int code_ally      = 0xF5;
const static int code_wait2     = 0xF6;
const static int code_enemy     = 0xF7;
const static int code_num       = 0xF8;
const static int code_scroll    = 0xF9;
const static int code_scroll2   = 0xFA;
const static int code_money     = 0xFB;
const static int code_nop       = 0xFC;
const static int code_spell     = 0xFD;
const static int code_item      = 0xFE;
const static int code_end       = 0xFF;

// added for translation
const static int code_tilebr  = 0xEE;

const static int code_charname0 = 0xF000;
const static int code_charname1 = 0xF001;
const static int code_charname2 = 0xF002;
const static int code_charnames_preset_base = 0xF003;

const static int maxAlphNumCharW = 5;
const static int tileW = 8;

const static int maxPlayerNameLen = 49; // all Ms or Ws = 7 * 7
// worst case: "universal will" = 57 pixels + 9 pixel enumeration
// if enumeration is disabled for universal will, this will instead
// be 65 pixels (for a 7-tile name + enumeration).
// for all the difference it makes.
const static int maxAllyNameLen   = 66;
const static int maxEnemyNameLen  = 66;
const static int maxNumLen        = tileW * 5;
const static int maxMoneyLen      = tileW * 7;
const static int maxSpellNameLen  = tileW * 7;
const static int maxItemNameLen   = (tileW * 7) + 1;

LastBibleLineWrapper::LastBibleLineWrapper(BlackT::TStream& src__,
                ResultCollection& dst__,
                const BlackT::TThingyTable& thingy__,
                CharSizeTable sizeTable__,
                int xSize__,
                int ySize__)
  : TLineWrapper(src__, dst__, thingy__, xSize__, ySize__),
    sizeTable(sizeTable__),
    xBeforeWait(-1),
    yBeforeWait(-1),
    trueY(0),
//    clearMode(clearMode_default),
    breakMode(breakMode_single),
    boxMode(boxMode_1),
    waitPending(false),
    boxFilled(false) {
  
}

int LastBibleLineWrapper::widthOfKey(int key) {
  if ((key == code_stringbuf))
    return tileW * 12;
  else if ((key == code_br))
    return 0;
  else if ((key == code_end))
    return 0;
  else if ((key == code_tilebr))
//    return 8;  // assume worst case
    return tileW - (xPos % tileW);
  else if (key == code_ally)
    return maxAllyNameLen;
  else if (key == code_enemy)
    return maxEnemyNameLen;
  else if (key == code_num)
    return maxNumLen;
  else if (key == code_money)
    return maxMoneyLen;
  else if (key == code_spell)
    return maxSpellNameLen;
  else if (key == code_item)
    return maxItemNameLen;
  else if ((key == code_charname0)
           || (key == code_charname1)
           || (key == code_charname2))
    return maxPlayerNameLen;
  else if (key >= code_charnames_preset_base)
    return maxAllyNameLen;
  else if ((key >= controlOpsStart) && (key < controlOpsEnd))
    return 0;
  
  return sizeTable[key];
}

bool LastBibleLineWrapper::isWordDivider(int key) {
  if (
      (key == code_br)
      || (key == code_space)
     ) return true;
  
  return false;
}

bool LastBibleLineWrapper::isLinebreak(int key) {
  if (
      (key == code_br)
      ) return true;
  
  return false;
}

bool LastBibleLineWrapper::isBoxClear(int key) {
  if ((key == code_end)
      || (key == code_clear)
      || (key == code_wait)
      || (key == code_wait2)
      ) return true;
  
  return false;
}

void LastBibleLineWrapper::onBoxFull() {
/*  if (clearMode == clearMode_default) {
    std::string content;
    if (lineHasContent) {
      // wait
      content += thingy.getEntry(code_wait);
      content += thingy.getEntry(code_br);
      currentScriptBuffer.write(content.c_str(), content.size());
    }
    // linebreak
    stripCurrentPreDividers();
    
    currentScriptBuffer.put('\n');
    xPos = 0;
    yPos = 0;
  }
  else if (clearMode == clearMode_messageSplit) {
    std::string content;
//      if (lineHasContent) {
      // wait
//        content += thingy.getEntry(code_wait);
//        content += thingy.getEntry(code_br);
      content += thingy.getEntry(code_end);
      content += "\n\n#ENDMSG()\n\n";
      currentScriptBuffer.write(content.c_str(), content.size());
//      }
    // linebreak
    stripCurrentPreDividers();
    
    xPos = 0;
    yPos = 0;
  } */
  
//  boxFilled = true;
  
  std::string content;
  if (lineHasContent) {
    // wait
    if (boxMode == boxMode_1) {
      content += thingy.getEntry(code_wait);
    }
    else if (boxMode == boxMode_2) {
      content += thingy.getEntry(code_wait2);
    }
    content += linebreakString();
    currentScriptBuffer.write(content.c_str(), content.size());
  }
  // linebreak
  stripCurrentPreDividers();
  
  currentScriptBuffer.put('\n');
  xPos = 0;
//  yPos = -1;
  yPos = 0;

/*  std::cerr << "WARNING: line " << lineNum << ":" << std::endl;
  std::cerr << "  overflow at: " << std::endl;
  std::cerr << streamAsString(currentScriptBuffer)
    << std::endl
    << streamAsString(currentWordBuffer) << std::endl; */
}

//int LastBibleLineWrapper::linebreakKey() {
//  return code_br;
//}

std::string LastBibleLineWrapper::linebreakString() const {
  std::string breakString = thingy.getEntry(code_br);
/*  if (breakMode == breakMode_single) {
    return breakString;
  }
  else {
    return breakString + breakString;
  } */
  
//  if (boxFilled) {
  if ((ySize != -1) && (trueY >= (ySize - 1))) {
    if (boxMode == boxMode_1) {
      return thingy.getEntry(code_br) + thingy.getEntry(code_scroll);
    }
    else if (boxMode == boxMode_2) {
      return thingy.getEntry(code_br) + thingy.getEntry(code_scroll2);
    }
  }
  else {
    return thingy.getEntry(code_br);
  }
}

void LastBibleLineWrapper::outputLinebreak(std::string str) {
  ++trueY;
  TLineWrapper::outputLinebreak(str);
}

//int LastBibleLineWrapper::linebreakHeight() const {
//  if (breakMode == breakMode_single) {
//    return 1;
//  }
//  else {
//    return 2;
//  }
//}

void LastBibleLineWrapper::onSymbolAdded(BlackT::TStream& ifs, int key) {
  
/*  if (!waitPending
      && ((key == code_wait) || (key == code_wait2))) {
    // inject linebreak after manual wait if content remains
    waitPending = true;
    flushActiveWord();
//    yPos = -1;
  }
  // if a manual wait previously occurred, and we're adding any kind of
  // content to the box, output a linebreak first
  else */
  if (waitPending) {
    if ((key != code_end)
        && (key != code_clear)) {
//      std::cerr << "here: " << std::hex << key << std::endl;
      outputLinebreak(linebreakString());
    }
    
    waitPending = false;
  }
  
  // if box previously filled, and will now be reset, mark as unfilled
//  if (boxFilled
//      && ((key == code_end) || (key == code_clear))) {
//    boxFilled = false;
//  }
  
/*  if (isLinebreak(key)) {
    if ((yPos != -1) && (yPos >= ySize - 1)) {
      flushActiveWord();
      
    }
  } */
}

void LastBibleLineWrapper
    ::handleManualLinebreak(TLineWrapper::Symbol result, int key) {
//  if ((key != code_br) || (breakMode == breakMode_single)) {
//  }
//  else {
//    outputLinebreak(linebreakString());
//  }
  if (key == code_br) {
    outputLinebreak(linebreakString());
  }
  else {
    TLineWrapper::handleManualLinebreak(result, key);
  }
}

void LastBibleLineWrapper::afterLinebreak(
    LinebreakSource clearSrc, int key) {
/*  if (clearSrc != linebreakBoxEnd) {
    if (spkrOn) {
      xPos = spkrLineInitialX;
    }
  } */
  
/*  if (clearSrc == linebreakManual) {
    if (breakMode == breakMode_double) {
      --yPos;
    }
  } */
}

void LastBibleLineWrapper::beforeBoxClear(
    BoxClearSource clearSrc, int key) {
  if ((clearSrc == boxClearManual)
      && ((key == code_wait) || (key == code_wait2))) {
    xBeforeWait = xPos;
    yBeforeWait = yPos;
  }
  else if ((clearSrc == boxClearManual)
      && ((key == code_clear) || (key == code_end))) {
    trueY = 0;
  }
}

void LastBibleLineWrapper::afterBoxClear(
  BoxClearSource clearSrc, int key) {
  if (((clearSrc == boxClearManual)
      && ((key == code_wait) || (key == code_wait2)))) {
//    xPos = xBeforeWait;
    yPos = -1;

    // y is preserved if box was "cleared" with wait.
    // why was box scrolling implemented like this, it breaks all my
    // line wrapping assumptions
//    yPos = yBeforeWait;

    // inject linebreak after manual wait if content remains
    waitPending = true;
  }
}

bool LastBibleLineWrapper::processUserDirective(BlackT::TStream& ifs) {
  TParse::skipSpace(ifs);
  
  std::string name = TParse::matchName(ifs);
  TParse::matchChar(ifs, '(');
  
  for (int i = 0; i < name.size(); i++) {
    name[i] = toupper(name[i]);
  }
  
  if (name.compare("SETBREAKMODE") == 0) {
    std::string type = TParse::matchName(ifs);
    
    if (type.compare("SINGLE") == 0) {
      breakMode = breakMode_single;
    }
    else if (type.compare("DOUBLE") == 0) {
      breakMode = breakMode_double;
    }
    else {
      throw TGenericException(T_SRCANDLINE,
                              "LastBibleLineWrapper::processUserDirective()",
                              "Line "
                                + TStringConversion::intToString(lineNum)
                                + ": unknown break mode '"
                                + type
                                + "'");
    }
    
    return true;
  }
  else if (name.compare("SETBOXMODE") == 0) {
    std::string type = TParse::matchName(ifs);
    
    if (type.compare("MODE1") == 0) {
      boxMode = boxMode_1;
    }
    else if (type.compare("MODE2") == 0) {
      boxMode = boxMode_2;
    }
    else {
      throw TGenericException(T_SRCANDLINE,
                              "LastBibleLineWrapper::processUserDirective()",
                              "Line "
                                + TStringConversion::intToString(lineNum)
                                + ": unknown box mode '"
                                + type
                                + "'");
    }
    
    return true;
  }
/*  else if (name.compare("PARABR") == 0) {
//    if (yPos >= ySize) {
//      onBoxFull();
//    }
//    else {
//      onBoxFull();
//    }
    flushActiveWord();
    outputLinebreak();
    return true;
  } */
//  else if (name.compare("ENDMSG") == 0) {
//    processEndMsg(ifs);
//    return true;
//  }
  
  return false;
}

}
