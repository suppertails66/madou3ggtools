#ifndef BSSMCMP_H
#define BSSMCMP_H


#include "util/TStream.h"
#include "util/TBufStream.h"
#include "util/TByte.h"

namespace Sms {

class BssmCmp {
public:
  static void decmpBssm(BlackT::TStream& src, BlackT::TStream& dst);
protected:
  const static int minAbsoluteRun = 2;
// const static int maxAbsoluteRun = 0xFF + 1;
  const static int maxAbsoluteRun = 0x7F + 1;
  const static int maxLookbackRun = 0xF + 3;
  const static int minLookbackRun = 3;
  const static int maxLookbackDistance = 0x7FF;
  
  const static int lookbackSize = 0x800;
  const static int lookbackMask = 0x7FF;
  
  friend class BssmDecompressor;
};

class BssmDecompressor {
public:
  BssmDecompressor(BlackT::TStream& src__, BlackT::TStream& dst__);
  
  void operator()();
protected:
  BlackT::TStream& src;
  BlackT::TStream& dst;
//  int inputSize;
  int planeSize;
//  BlackT::TBufStream lookbackBuffer;

  void decmpPlane(BlackT::TStream& ifs, BlackT::TStream& ofs);
};


}


#endif
