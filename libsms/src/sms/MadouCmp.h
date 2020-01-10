#ifndef MADOUCMP_H
#define MADOUCMP_H


#include "util/TStream.h"
#include "util/TBufStream.h"

namespace Sms {

class MadouCmp {
public:
//  static int cmpMadou(BlackT::TStream& src, BlackT::TStream& dst);
  static void decmpMadou(BlackT::TStream& src, BlackT::TStream& dst);
protected:
/*  const static int minAbsoluteRun = 2;
// const static int maxAbsoluteRun = 0xFF + 1;
  const static int maxAbsoluteRun = 0x7F + 1;
  const static int maxLookbackRun = 0xF + 3;
  const static int minLookbackRun = 3;
  const static int maxLookbackDistance = 0x7FF;

  struct LookbackCommand {
    int offset;
    int length;
  };
  
  static void flushAbsoluteBuffer(BlackT::TBufStream& absoluteBuffer,
                           BlackT::TStream& dst);
  
  static LookbackCommand findBestLookback(BlackT::TStream& src); */
  
};


}


#endif
