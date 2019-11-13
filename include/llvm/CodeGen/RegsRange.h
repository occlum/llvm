//===- llvm/CodeGen/RegRange.h - Register Range Set ----------*- C++ -*-===//
//
/// \file
/// A set of register ranges. It is intended for backend range analysis.
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_CODEGEN_REGSRANGE_H
#define LLVM_CODEGEN_REGSRANGE_H

#include "llvm/CodeGen/MachineRegisterInfo.h"
#include "llvm/CodeGen/TargetRegisterInfo.h"
#include "llvm/MC/LaneBitmask.h"
#include "llvm/MC/MCRegisterInfo.h"
#include <memory>

using std::unique_ptr;

namespace llvm {

#define DEBUG_TYPE "RegRange"
class MachineInstr;
class MachineBasicBlock;
class raw_ostream;

enum Range {
  InRead,
  InWrite,
  SmallNum,
  Undef,
  Unknown,
};
class RangeInfo {
public:
  Range RangeClass = Undef;
  int UpRange;
  int LowRange;

  RangeInfo(Range R, int up, int low) {
    RangeClass = R;
    UpRange = up;
    LowRange = low;
  }
  RangeInfo(Range R) {
    RangeClass = R;
    UpRange = 0;
    LowRange = 0;
  }
  RangeInfo() {
    RangeClass = Undef;
    UpRange = 0;
    LowRange = 0;
  }
  /* RangeInfo() = delete; */

  RangeInfo(const RangeInfo &R) {
    RangeClass = R.RangeClass;
    UpRange = R.UpRange;
    LowRange = R.LowRange;
  }

  bool min(RangeInfo R) {
    if (R.RangeClass != SmallNum)
      return false;
    if (RangeClass == Undef || RangeClass == Unknown)
      return false;
    UpRange = UpRange - R.UpRange;
    LowRange = LowRange - R.LowRange;
    return true;
  }

  bool add(RangeInfo R) {
    if (R.RangeClass != SmallNum)
      return false;
    if (RangeClass == Undef || RangeClass == Unknown)
      return false;
    UpRange = UpRange + R.UpRange;
    LowRange = LowRange + R.LowRange;
    return true;
  }

  bool multiple(int i) {
    if (RangeClass != SmallNum)
      return false;
    UpRange = UpRange * i;
    LowRange = LowRange * i;
    return true;
  }

  bool add(int i) {
    if (RangeClass == Undef || RangeClass == Unknown)
      return false;
    UpRange = UpRange + i;
    LowRange = LowRange + i;
    return true;
  }

  bool isInRange(const RangeInfo &R) const {
    if (RangeClass == Undef || RangeClass == Unknown)
      return false;
    if (RangeClass == R.RangeClass) {
      if (UpRange < R.UpRange && LowRange > R.LowRange)
        return true;
      else
        return false;
    }

    // FIXME
    // depending on READ/WRITE region size
    if (RangeClass == InWrite && R.RangeClass == InRead) {
      return true;
    }
    return false;
  }

  bool equal(const RangeInfo R) {
    if (RangeClass != R.RangeClass) {
      return false;
    }
    if (UpRange != R.UpRange || LowRange != R.LowRange) {
      return false;
    }
    return true;
  }

  // Lattic meet operation
  //      ---------
  //      | Undef |
  //      ---------
  // ---------
  // |InWrite|
  // ---------  ----------
  // |InRead |  |SmallNum|
  // ---------  ----------
  //      ---------
  //      |Unknown|
  //      ---------
  //
  //  meet will extend it's range
  static RangeInfo meet(RangeInfo &A, RangeInfo &B) {
    if (A.RangeClass == Undef) {
      return B;
    } else if (B.RangeClass == Undef) {
      return A;
    }
    if (A.RangeClass == Unknown || B.RangeClass == Unknown) {
      return RangeInfo(Unknown);
    }
    if (A.RangeClass == B.RangeClass) {
      int up = A.UpRange > B.UpRange ? A.UpRange : B.UpRange;
      int low = A.LowRange < B.LowRange ? A.LowRange : B.LowRange;
      return RangeInfo(A.RangeClass, up, low);
    }
    if (A.RangeClass == InWrite && B.RangeClass == InRead) {
      return B;
    }
    if (A.RangeClass == InRead && B.RangeClass == InWrite) {
      return A;
    }
    return RangeInfo(Unknown);
  }
  // cut current range to bound check's Range.
  // if current range is exceed guard's range, cut it.
  // otherwise, return current range;
  static RangeInfo cutRange(RangeInfo &current, RangeInfo &guard) {
    if (guard.RangeClass == current.RangeClass) {
      // get lower one for up
      int up =
          current.UpRange > guard.UpRange ? guard.UpRange : current.UpRange;
      // get higher one for low
      int low =
          current.LowRange < guard.LowRange ? guard.LowRange : current.LowRange;
      return RangeInfo(current.RangeClass, up, low);
    }
    // If current and guard are not in the same RangeClass, then if current is
    // smaller, then return current
    if (current.RangeClass == InWrite) {
      return current;
    }
    // other situation return guard
    return guard;
  }
  void print(raw_ostream &OS) const {
    OS << "RangeInfo:";
    switch (RangeClass) {
    case Undef:
      OS << " Undef\n";
      break;
    case Unknown:
      OS << " Unknown\n";
      break;
    case InWrite:
      OS << " InWrite, Up: " << UpRange << " Low: " << LowRange << "\n";
      break;
    case InRead:
      OS << " InRead, Up: " << UpRange << " Low: " << LowRange << "\n";
      break;
    case SmallNum:
      OS << " Smallnum, Up: " << UpRange << " Low: " << LowRange << "\n";
      break;
    default:
      break;
    }
  }
};
inline raw_ostream &operator<<(raw_ostream &OS, const RangeInfo &LR) {
  LR.print(OS);
  return OS;
}
class RegsRange {

  const TargetRegisterInfo *TRI = nullptr;

public:
  std::map<unsigned, RangeInfo> Ranges;
  RegsRange(const TargetRegisterInfo &TRI) { init(TRI); }
  RegsRange() = delete;

  void init(const TargetRegisterInfo &TRI) {
    this->TRI = &TRI;
    Ranges.clear();
  }

  // RegsRange A equals B iif every Range in A equal B's Range
  // and every Range in B equal A's Range
  bool equal(RegsRange &R) {
    for (auto &RegIt : Ranges) {
      if (!RegIt.second.equal(R.Ranges[RegIt.first])) {
        return false;
      }
    }
    for (auto &RegIt : R.Ranges) {
      if (RegIt.second.RangeClass == Undef)
        continue;
      LLVM_DEBUG(dbgs() << TRI->getRegAsmName(RegIt.first) << "\nR.range "
                        << RegIt.second);
      LLVM_DEBUG(dbgs() << "range " << Ranges[RegIt.first]);
      if (!RegIt.second.equal(Ranges[RegIt.first])) {
        return false;
      }
      LLVM_DEBUG(dbgs() << "equals \n");
    }
    LLVM_DEBUG(dbgs() << "return equals \n");
    return true;
  }

  void setAllUnknown() {
    for (auto &RegIt : Ranges) {
      if (RegIt.second.RangeClass != Undef)
        RegIt.second = RangeInfo(Unknown);
    }
  }

  // compare this with R, if any difference, set to unknown
  // set Changed Unknown should always
  void setChangedUnknown(RegsRange &R) {
    LLVM_DEBUG(dbgs() << "setChangedUnknown\t");
    for (auto &RegIt : Ranges) {
      // do not be undef, so avoid register alias
      if (RegIt.second.RangeClass != Undef) {
        if (!RegIt.second.equal(R.Ranges[RegIt.first])) {
          LLVM_DEBUG(dbgs() << TRI->getRegAsmName(RegIt.first) << "\t");
          RangeInfo r(Unknown);
          // use setRegRange so if R use different register, the programe won't
          // set two overlaped register
          setRegRange(RegIt.first, r);
        }
      }
    }
    LLVM_DEBUG(dbgs() << "\n");
  }

  // get a reg range of Reg.
  RangeInfo getRegRange(unsigned Reg) {
    LLVM_DEBUG(dbgs() << "getRegRange \t");
    for (MCSubRegIterator SubRegs(Reg, TRI, true); SubRegs.isValid(); ++SubRegs)
      if (Ranges[*SubRegs].RangeClass != Undef) {
        LLVM_DEBUG(dbgs() << TRI->getRegAsmName(*SubRegs) << "\t");
        LLVM_DEBUG(dbgs() << Ranges[*SubRegs] << "\t");
        return Ranges[*SubRegs];
      }
    // if any of the super register has a range, then set it's range to
    // unknown
    for (MCSuperRegIterator SuperRegs(Reg, TRI, false); SuperRegs.isValid();
         ++SuperRegs) {
      LLVM_DEBUG(dbgs() << TRI->getRegAsmName(*SuperRegs) << "\t");
      LLVM_DEBUG(dbgs() << Ranges[*SuperRegs] << "\t");
      if (Ranges[*SuperRegs].RangeClass != Undef)
        return RangeInfo(Unknown);
    }
    // else return Undef
    return Ranges[Reg];
  }

  // This function will set all Reg's related range
  // When the register is new defined, set it's range as fresh
  // Like Move m, Reg
  void setRegRange(unsigned Reg, RangeInfo &Range) {
    LLVM_DEBUG(dbgs() << "setRegRange: \n");
    for (MCSubRegIterator SubRegs(Reg, TRI, false); SubRegs.isValid();
         ++SubRegs) {
      LLVM_DEBUG(dbgs() << TRI->getRegAsmName(*SubRegs) << "\t");
      Ranges[*SubRegs] = RangeInfo(Undef);
      LLVM_DEBUG(dbgs() << Ranges[*SubRegs] << "\t");
    }
    for (MCSuperRegIterator SuperRegs(Reg, TRI, false); SuperRegs.isValid();
         ++SuperRegs) {
      LLVM_DEBUG(dbgs() << TRI->getRegAsmName(*SuperRegs) << "\t");
      Ranges[*SuperRegs] = RangeInfo(Undef);
      LLVM_DEBUG(dbgs() << Ranges[*SuperRegs] << "\t");
    }
    LLVM_DEBUG(dbgs() << TRI->getRegAsmName(Reg) << "\n");
    Ranges[Reg] = Range;
    LLVM_DEBUG(dbgs() << Ranges[Reg] << "\t");
  }

  // merge r into this regsrange
  void merge(RegsRange &r) {
    for (auto &RegIt : r.Ranges) {
      // find all ranges in this range
      if (RegIt.second.RangeClass != Undef) {
        bool found;
        // find subregister in this with Ranges
        for (MCSubRegIterator SubRegs(RegIt.first, TRI, true);
             SubRegs.isValid(); ++SubRegs) {
          if (this->Ranges[*SubRegs].RangeClass != Undef) {
            this->Ranges[*SubRegs] =
                RangeInfo::meet(Ranges[*SubRegs], RegIt.second);
            found = true;
            break;
          }
        }
        // if found r go to next register
        if (found)
          continue;
        // find supregister in r with Ranges
        for (MCSuperRegIterator SuperRegs(RegIt.first, TRI, false);
             SuperRegs.isValid(); ++SuperRegs) {
          if (this->Ranges[*SuperRegs].RangeClass != Undef) {
            this->Ranges[*SuperRegs] = RangeInfo(Undef);
            this->Ranges[RegIt.first] =
                RangeInfo::meet(Ranges[*SuperRegs], RegIt.second);
            found = true;
            break;
          }
        }
        if (!found) {
          this->Ranges[RegIt.first] = RegIt.second;
        }
      }
    }
  }

  void print(raw_ostream &OS) const {
    OS << "RegsRange: \n";
    for (auto &RegIt : this->Ranges) {
      if (RegIt.second.RangeClass != Undef)
        OS << TRI->getRegAsmName(RegIt.first) << " " << RegIt.second;
    }
    OS << "\n";
  }
};

inline raw_ostream &operator<<(raw_ostream &OS, const RegsRange &LR) {
  LR.print(OS);
  return OS;
}
} // end of namespace llvm
#undef DEBUG_TYPE
#endif
