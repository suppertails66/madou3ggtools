#include "sms/GodzillaCmp.h"
#include "util/TBufStream.h"
#include <vector>
#include <iostream>

using namespace BlackT;

namespace Sms {


void GodzillaCmp::decmpGodzilla(BlackT::TStream& src, BlackT::TStream& dst) {
  GodzillaDecompressor(src, dst)();
}

GodzillaDecompressor::GodzillaDecompressor(
    BlackT::TStream& src__, BlackT::TStream& dst__)
  : src(src__), dst(dst__),
    remaining(0),
    lookbackBuffer(GodzillaCmp::lookbackSize) {
//  lookbackBuffer.padToSize(GodzillaCmp::lookbackSize);
//  lookbackBuffer.seek(0);

  // buffer must be zero-initialized
  for (int i = 0; i < GodzillaCmp::lookbackSize; i++)
    lookbackBuffer.put(0);
  lookbackBuffer.seek(0);
}

void GodzillaDecompressor::operator()() {
  // read input size
  remaining = src.readu16le();
  
  while (remaining > 0) {
//    std::cerr << std::hex << src.tell() << std::endl;
    int cmd = (unsigned char)src.peek();
    
    // lookback
         if ((cmd & 0x80) == 0) doLookback();
    else if ((cmd & 0x40) == 0) doAbsolute();
    else if ((cmd & 0x20) == 0) doRepeatedAbsoluteShort();
    else                        doRepeatedAbsoluteLong();
  }
}

BlackT::TByte GodzillaDecompressor::fetchByte() {
  --remaining;
  return src.get();
}

BlackT::TByte GodzillaDecompressor::getLookbackByte(int pos) {
  int oldPos = lookbackBuffer.tell();
  lookbackBuffer.seek(pos & GodzillaCmp::lookbackMask);
  BlackT::TByte result = lookbackBuffer.get();
  lookbackBuffer.seek(oldPos);
  return result;
}

void GodzillaDecompressor::outputByte(BlackT::TByte b) {
  dst.put(b);
  
  outputLookbackByte(b);
}

void GodzillaDecompressor::outputLookbackByte(BlackT::TByte b) {
  lookbackBuffer.put(b);
  if (lookbackBuffer.tell() >= GodzillaCmp::lookbackSize)
    lookbackBuffer.seek(0);
}

void GodzillaDecompressor::doLookback() {
  int high = fetchByte();
  int low = fetchByte();
  
  int distance = (high << 4) | ((low >> 4) & 0x0F);
  int length = (low & 0x0F) + 2;
  
  int baseLookbackPos = lookbackBuffer.tell();
  // no need to mask this or worry about overflow;
  // getLookbackByte() takes care of it
  int offset = (baseLookbackPos - distance);
  
  TBufStream temp(length);
  for (int i = 0; i < length; i++) {
    BlackT::TByte b = getLookbackByte(offset);
//    outputByte(b);
    
    // all lookback bytes are output before the lookback buffer is updated
    dst.put(b);
    temp.put(b);
    
    ++offset;
  }
  
  // update lookback buffer
  temp.seek(0);
  for (int i = 0; i < length; i++) {
    outputLookbackByte(temp.get());
  }
}

void GodzillaDecompressor::doAbsolute() {
  int count = (fetchByte() & 0x3F) + 1;
  for (int i = 0; i < count; i++) {
    outputByte(fetchByte());
  }
}

void GodzillaDecompressor::doRepeatedAbsoluteShort() {
  int rawFirst = fetchByte();
  int rawRepCount = (rawFirst & 0x07);
  
  int rawLen = (rawFirst >> 3) & 0x03;
  
  doRepeatedAbsolute(rawLen, rawRepCount);
}

void GodzillaDecompressor::doRepeatedAbsoluteLong() {
  int rawFirst = fetchByte();
  int rawRepCount = (rawFirst & 0x07);
  rawRepCount <<= 8;
  rawRepCount |= fetchByte();
  
  int rawLen = (rawFirst >> 3) & 0x03;
  
  doRepeatedAbsolute(rawLen, rawRepCount);
}

void GodzillaDecompressor::doRepeatedAbsolute(int rawLen, int rawRepCount) {
  int length = rawLen + 1;
  int repCount = rawRepCount + 2;
  
  TBufStream buffer(length);
  for (int i = 0; i < length; i++) {
    buffer.put(fetchByte());
  }
  
  for (int i = 0; i < repCount; i++) {
    buffer.seek(0);
    for (int j = 0; j < length; j++) {
      outputByte(buffer.get());
    }
  }
}


}
