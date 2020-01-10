
;.enum $CD00
.enum $DC00
  vwfAllocationArray    .db  ; bytes in this array are nonzero if
                                        ; the corresponding tile has been
                                        ; allocated for display
  vwfBuffer             ds bytesPerTile ; tiles are composed here before going
                                        ; to the VDP
  
  vwfAllocationArraySize .db             ; number of currently used VWF tiles
                                        ; out of the maximum
  vwfAllocationArrayPos .db              ; most recently assigned index in
                                        ; the VWF alloc array
  vwfAllocationArrayBaseTile .dw         ; base tile index the vwf alloc array is
                                        ; targeting
    vwfAllocationArrayBaseTileLo .db
    vwfAllocationArrayBaseTileHi .db
  assumeScrollZeroFlag  .db              ; when set, assume nametable scroll is
                                        ; zero for purpose of computing
                                        ; screen-local coordinates
  vwfNametableHighMask      .db          ; OR mask applied to high byte of
                                        ; VWF nametable output
  
  inlinePrintNum        .dw      ; number printed when an inline_num op
                                ; is encountered
  inlinePrintDigitCount .db      ; number of digits in inline print
                                ; (00 = don't care)
  inlinePrintShowLeadingZeroes .db
  
  numberPrintBuffer     .db    ; buffer for printing script-inline numbers
  numberConvBuffer      .db    ; buffer for temp BCD conversion storage
  
  lastPrintOffsetXY     .dw
    lastPrintOffsetY      .db
    lastPrintOffsetX      .db
  lastPrintBaseXY       .dw
    lastPrintBaseY        .db
    lastPrintBaseX        .db
  
  windowBgTypeIndex     .db      ; temporary storage for window type during
                                ; the process of opening it, which we need
                                ; to preserve longer than the original game
                                ; in order to check whether extended VWF
                                ; is necessary
  vwfStateSuspended     .db      ; nonzero if extended VWF active
  vwfSuspendWindowPending       .db      ; nonzero if extended VWF pending...
                                        ; i think
  
  stringBuffer       .db    ; buffer for prepping stuff (mostly names) for
                              ; RAM printing
    stringBufferEnd       .db
  
  printSkipCounter      .db      ; a counter incremented after each character
                                ; in a default-speed print is printed.
                                ; used to speed up printing to multiple
                                ; characters per frame.
  needRightBorderFlag   .db      ; set when a print targeted at a window
                                ; border is detected so the string print
                                ; routine knows to fill in the last character
                                ; up to the end of the tile
  
; FIXME: NOT IMPLEMENTED
  vwfLocalHalfPrintFlag .db
  
  nametableBuffer    .db      ; buffer for certain nametable prints
  copyFullCharNameFlag  .db      ; set to copy full character names to nametable
                                ; instead of limiting to 7 tiles
  
  scrollSourceBank      .db      ; text crawl source data bank
  scrollGrpTilesPointer .dw      ; pointer to text crawl graphics data
                                ; as it's being loaded
  scrollTilemapPointer  .dw      ; pointer to text crawl tilemap data
                                ; as it's being loaded
  scrollGrpTilesLeft    .dw      ; number of tiles remaining to be loaded
  scrollTilemapsLeft    .db      ; number of tilemaps left in sequence
  scrollGrpCurDstTile   .dw      ; current target tile number when loading
                                ; graphics
  
  specialPrintWaitCounter db    ; intro printing: delay to next char
  specialPrintPointer   .dw      ; pointer to intro string
  specialPrintBank      .db      ; intro string bank
  
  speechEmulationDelayAmount .db ; frames to delay for current speech
                                ; emulation pause
  speechEmulationCharsLeft .db   ; repeat delay for this many characters
                                ; before turning it off.
                                ; this is predecremented; set to 2 to print
                                ; 1 character, etc.
;  speechEmulationDelayedDelayCounter db      ; when e.g. an ellipsis is
                                ; detected, this is set to the number
                                ; of characters that should be ignored
                                ; before triggering it
  textFastForwardFlag   .db
  
  
  
  
  
  ; 
  ENDOFGARBAGE          dw
  
  
  
  
  
  
                                        
  vwfFullDeallocFlag    .db
  noInterruptDisableFlag db
  
  vwfBufferPending      db              ; nonzero if buffer not empty and
                                        ; not flushed
  vwfPixelOffset        db              ; currently target pixel position in
                                        ; VWF buffer
  vwfTransferCharSize   db
  vwfTransferRight_leftShift    db
  
  vwfLocalTargetFlag        db          ; if true, print nametable data to RAM
                                        ; rather than directly to VDP
    vwfLocalTargetBaseAddr      dw      ; base address of local nametable
                                        ; target (RAM address of nametable data
                                        ; for upper-left character in printable
                                        ; area)
    vwfLocalTargetW  db                 ; width in tiles of one line in the
                                        ; local nametable target, for
                                        ; linebreak calculations
    vwfLocalTargetH  db                 ; height in tiles of the
                                        ; local nametable target,
                                        ; for garbage collection checks
    vwfLocalTargetCurrLineAddr      dw    ; address of current target line
                                          ; during printing
  
  printBaseXY           .dw
    printBaseY            .db
    printBaseX            .db
  printOffsetXY         .dw
    printOffsetY          .db
    printOffsetX          .db
  printAreaWH           .dw
    printAreaH            .db
    printAreaW            .db
  
  scratch               .dw
    scratchLo             db
    scratchHi             db
  
  bankedCallA           db
  bankedCallHL          dw
  
  
  
  
  
  
  ; madou stuff
  
  
  vwfExpRamTileCurAddr .dw    ; index of currently allocated
                                ; buffer tile (zero if none)
  vwfExpRamTileBaseAddr dw      ; base address to start putting tile data
                                ; at when a new string is started.
                                ; must be in expansion ram.
  vwfExpRamTileNextAddr  dw      ; current target address for the above
  vwfCurTileIndexLo     db      ; low byte of current target vwf tile.
                                ; high assumed to be 1
  vwfPendingTileCounter db      ; number of graphic patterns to send to
                                ; vdp
  vwfTilemapTargetAddr  dw
  
  vwfCbcActiveFlag      db
  ; FIXME: this should have been db
  vwfCbcScriptBank      dw      ; bank of char-by-char script data
  vwfCbcScriptAddr      dw      ; address of char-by-char script data
  vwfCbcVramTarget      dw
  vwfCbcVramNextTarget  dw
  extraSceneTimer       db      ; counter used to reduce tick rate of scene timer
  
  boxNewSpaceVdpAddr    .dw
    paramWord1            dw
  boxOldSpaceEndTileNum .dw
    paramWord2            dw
  boxOldSpaceEndVdpAddr .dw
    paramWord3            dw
  
  cbcSyncSubTimer       db
  subsceneTimerInterpolationCounter       db
  
;  noBoxReset            db
;  ramScriptBuffer       ds $40
  
.ende
                                        
.enum $A000
  ; temp storage for VWF and sprite tiles that are overwritten when
  ; full-screen menus (e.g. item screen) are brought up
  expRam_tileBuffer             ds $1DE0
  
  ; vwf params
  expRam_vwfAllocationArraySize         db
  expRam_vwfAllocationArrayPos          db
  expRam_vwfAllocationArrayBaseTile     .dw
    expRam_vwfAllocationArrayBaseTileLo db
    expRam_vwfAllocationArrayBaseTileHi db
  expRam_assumeScrollZeroFlag           db
  expRam_vwfNametableHighMask           db
  
  ; buffer to hold temporary backup of VWF tile allocation array for
  ; e.g. the above menus, or when the buffer would otherwise get overwritten
  ; (e.g. graphic decompression)
  expRam_vwfAllocationArrayBackup      ds maxVwfTiles
  
  ; for compression specifically, because of the stupid combine menu
  expRam_vwfAllocationArrayBackup_compression      ds maxVwfTiles
.ende

