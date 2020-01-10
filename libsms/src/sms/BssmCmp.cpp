#include "sms/BssmCmp.h"
#include "util/TBufStream.h"
#include <vector>
#include <iostream>

using namespace BlackT;

namespace Sms {


void BssmCmp::decmpBssm(BlackT::TStream& src, BlackT::TStream& dst) {
  BssmDecompressor(src, dst)();
}

BssmDecompressor::BssmDecompressor(
    BlackT::TStream& src__, BlackT::TStream& dst__)
  : src(src__), dst(dst__),
    planeSize(-1) {
  
}

void BssmDecompressor::operator()() {
  // read plane size
  planeSize = src.readu16le();
  
  TBufStream planeBuffer(planeSize);
  
  int dstStart = dst.tell();
  
  // do odd byteplane
  decmpPlane(src, planeBuffer);
  planeBuffer.seek(0);
  for (int i = 0; i < planeBuffer.size(); i++) {
    dst.seekoff(1);
    dst.put(planeBuffer.get());
  }
  
  // do even byteplane
  dst.seek(dstStart);
  planeBuffer.seek(0);
  decmpPlane(src, planeBuffer);
  planeBuffer.seek(0);
  for (int i = 0; i < planeBuffer.size(); i++) {
    dst.put(planeBuffer.get());
    dst.seekoff(1);
  }
  
/*  while (remaining > 0) {
//    std::cerr << std::hex << src.tell() << std::endl;
    int cmd = (unsigned char)src.peek();
    
    // lookback
         if ((cmd & 0x80) == 0) doLookback();
    else if ((cmd & 0x40) == 0) doAbsolute();
    else if ((cmd & 0x20) == 0) doRepeatedAbsoluteShort();
    else                        doRepeatedAbsoluteLong();
  } */
}

void BssmDecompressor::decmpPlane(BlackT::TStream& ifs, BlackT::TStream& ofs) {
  int outpos = 0;
  while (outpos < planeSize) {
    unsigned char next = ifs.get();
    int count = (next & 0x7F) + 1;
    
    if ((next & 0x80) == 0) {
      // absolute
      for (int i = 0; i < count; i++) ofs.put(ifs.get());
    }
    else {
      // repeat
      char value = ifs.get();
      for (int i = 0; i < count; i++) ofs.put(value);
    }
    
    outpos += count;
  }
}


}
