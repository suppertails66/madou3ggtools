#include "sms/MadouCmp.h"
#include "util/TBufStream.h"
#include <vector>
#include <iostream>

using namespace BlackT;

namespace Sms {

/*void MadouCmp::flushAbsoluteBuffer(BlackT::TBufStream& absoluteBuffer,
                         BlackT::TStream& dst) {
  if (absoluteBuffer.size() <= 0) return;
  
  // cmd
  dst.writeu8(0x00 | (absoluteBuffer.size() - 1));
  // data
  absoluteBuffer.seek(0);
  for (int i = 0; i < absoluteBuffer.size(); i++) {
    dst.put(absoluteBuffer.get());
  }
  
  // clear buffer
  absoluteBuffer = TBufStream(maxAbsoluteRun);
}

MadouCmp::LookbackCommand MadouCmp::findBestLookback(BlackT::TStream& src) {
  LookbackCommand result;
  result.offset = 0;
  result.length = 0;
  
  int srcCheckPos = src.tell();
  int srcCheckLen = (src.remaining() >= maxLookbackRun)
                      ? maxLookbackRun : src.remaining();
  
  int checkStartPos = src.tell() - maxLookbackDistance;
  int checkWindowSize = maxLookbackDistance;
  // if check window is smaller than maximum
  if (checkStartPos < 0) {
    checkWindowSize += checkStartPos;
    checkStartPos = 0;
  }
  
  TBufStream extendedSrc(maxLookbackDistance + maxLookbackRun);
  src.seek(checkStartPos);
  extendedSrc.writeFrom(src, checkWindowSize);
  
  for (int i = 0; i < checkWindowSize; i++) {
    
    int distance = checkWindowSize - i;
    
    // if end of potential lookback check goes past end of src, simulate
    // a max-length repeat, modifying the end of extendedSrc
    if (distance < maxLookbackRun) {
      int repeatPartSize = maxLookbackRun - distance;
      extendedSrc.seek(checkWindowSize);
      for (int j = 0; j < repeatPartSize; j++) {
        extendedSrc.seekoff(-distance);
        TByte next = extendedSrc.get();
        extendedSrc.seekoff(distance - 1);
        extendedSrc.put(next);
      }
    
    }
    
    // check extendedSrc against next data in src
    src.seek(srcCheckPos);
    extendedSrc.seek(i);
    for (int j = 0; j < srcCheckLen; j++) {
      if (extendedSrc.get() != src.get()) break;
      
      if (j + 1 > result.length) {
        result.length = j + 1;
        result.offset = distance;
      }
    }
    
    // if largest possible size found, we're done
    if (result.length == maxLookbackRun) break;
  }
  
  src.seek(srcCheckPos);
  return result;
}

int MadouCmp::cmpMadou(BlackT::TStream& src, BlackT::TStream& dst) {
  int baseOutPos = dst.tell();
  TBufStream absoluteBuffer(maxAbsoluteRun);
  
  while (!src.eof()) {
    // if pending absolute run reaches max size, flush
    if (absoluteBuffer.size() >= maxAbsoluteRun) {
      flushAbsoluteBuffer(absoluteBuffer, dst);
    }
    else {
      // check if best lookback command is long enough to be efficient
      LookbackCommand bestLookbackCmd = findBestLookback(src);
      
      if (bestLookbackCmd.length >= minLookbackRun) {
        // flush any existing absolute data
        flushAbsoluteBuffer(absoluteBuffer, dst);
        
        // do lookback command
        int cmd = 0x8000;
        cmd |= (bestLookbackCmd.length - 3) << 0;
        cmd |= (bestLookbackCmd.offset - 0) << 4;
        dst.writeu16be(cmd);
        
        // advance src
        src.seekoff(bestLookbackCmd.length);
      }
      else {
        // add to current absolute command
        absoluteBuffer.put(src.get());
      }
    }
  }
  
  int outputSize = dst.tell() - baseOutPos;
  return outputSize;
} */

void MadouCmp::decmpMadou(BlackT::TStream& src, BlackT::TStream& dst) {
  int basePos = dst.tell();
  while (src.peek() != 0) {
    int cmd = src.readu8();
    
    if (cmd == 0) break;
    
    if ((cmd & 0x80) != 0) {
      // lookback
      
      // size of copy
      int length = (cmd & 0x7F) + 3;
      
      // size of lookback
      int distance = src.readu8() + 1;
      
      for (int i = 0; i < length; i++) {
        // lookbacks beyond the start of the buffer are treated as zero
        if ((dst.tell() - distance) < basePos) {
          dst.put(0);
          continue;
        }
        
        dst.seekoff(-distance);
        char next = dst.get();
        dst.seek(dst.size());
        dst.put(next);
      }
    }
    else {
      // absolute
      for (int i = 0; i < cmd; i++) {
        dst.put(src.get());
      }
    }
  }
}


}
