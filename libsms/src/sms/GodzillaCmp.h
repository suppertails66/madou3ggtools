#ifndef GODZILLACMP_H
#define GODZILLACMP_H


#include "util/TStream.h"
#include "util/TBufStream.h"
#include "util/TByte.h"

namespace Sms {

class GodzillaCmp {
public:
  static void decmpGodzilla(BlackT::TStream& src, BlackT::TStream& dst);
protected:
  const static int minAbsoluteRun = 2;
// const static int maxAbsoluteRun = 0xFF + 1;
  const static int maxAbsoluteRun = 0x7F + 1;
  const static int maxLookbackRun = 0xF + 3;
  const static int minLookbackRun = 3;
  const static int maxLookbackDistance = 0x7FF;
  
  const static int lookbackSize = 0x800;
  const static int lookbackMask = 0x7FF;
  
  friend class GodzillaDecompressor;
};

class GodzillaDecompressor {
public:
  GodzillaDecompressor(BlackT::TStream& src__, BlackT::TStream& dst__);
  
  void operator()();
protected:
  BlackT::TStream& src;
  BlackT::TStream& dst;
//  int inputSize;
  int remaining;
  BlackT::TBufStream lookbackBuffer;
  
  BlackT::TByte fetchByte();
  BlackT::TByte getLookbackByte(int pos);
  void outputByte(BlackT::TByte b);
  void outputLookbackByte(BlackT::TByte b);
  
  void doLookback();
  void doAbsolute();
  void doRepeatedAbsoluteShort();
  void doRepeatedAbsoluteLong();
  void doRepeatedAbsolute(int rawLen, int rawRepCount);
};


}


#endif
