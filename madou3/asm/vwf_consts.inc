
;=====
; madou3
;=====


.define vwfNumberOp1DigitIndex $1B
.define vwfNumberOpIndex $1C
.define vwfNumberWithZeroIndex $1D
.define vwfCbcModeIndex $1E
.define vwfTileBrIndex $1F

.define vwfDigitBaseIndex $30

.define vwfBrIndex $C3
.define terminatorIndex $FF
.define tilemapBufferMaxSize $480

.define vwfExpRamTile_startAddr $8400
.define vwfCbcBaseTextVdpAddr $71C0

;=====
; madou3 routines
;=====

;.define waitVblank $0D3D
.define waitVblank $08C3
;.define runRegion0Script $2690
.define runRegion0Script $2701
.define runRegion1Script $26FD
.define runRegion2Script $26F9
.define runRegion3Script $26F5
.define runRegion4Script $26F1

.define runScript $270A
.define runScriptNoClear $26E6

.define handleNextScriptCmd $2787

; A = tile count
; HL = srcaddr (expram)
; DE = dstaddr
;.define sendPendingExpRamTiles $321E
.define sendPendingExpRamTiles $315A

.define sendRawGrpDataToVdp $3169

.define copyCharToFrontBuffer $29D9

;=====
; madou3 ram
;=====

; TODO: check if this is right
;.define vdpTransferFlags $C011
.define vdpTransferFlags $C031

/*.define expRamToVdpQueueSize $C018
.define expRamTransferDst $C01A
.define expRamTransferSrc $C01C
.define currentTextExpRamPos $C03F
.define currentTextTileIndex $C043
.define textWindowType $C044
.define textLineBaseDst $C046
.define pendingExpRamTileCount $C0B3
.define currentTextTilesExpRamTarget $C0B4
.define currentTextTilesVdpTarget $C0B6 */
.define expRamToVdpQueueSize $C01A
.define expRamTransferDst $C01C
.define expRamTransferSrc $C01E
.define tilemapSettingsFlags $C031
.define textWindowType $c058
.define textLineBaseDst $C05A
.define pendingExpRamTileCount $c05D
.define currentTextTilesExpRamTarget $c05e
.define currentTextTilesVdpTarget $c060
.define textTilemapBackBufferPtr $c062
.define oldCbcTimer $C064
.define currentTextExpRamPos $c078
.define currentTextTileIndex $c07c
.define cutsceneSubTimer $C340
.define cutsceneMainTimer $C341

.define tilemapDirtyArray $C040

;.define numberBufferStart $C051
.define numberBufferStart $c022
.define numberBufferSize 4

.define leftBoxOrigMaxTiles 21
.define rightBoxOrigMaxTiles 21
; these tiles are a part of the nametable that isn't used during regular
; gameplay, so they can safely be used for additional text in the standard
; left/right boxes
.define leftBoxNewSpaceTileNum $1EA
.define rightBoxNewSpaceTileNum $1F1
.define leftBoxNewSpaceVdpAddr ($1EA*bytesPerTile)|$4000
.define rightBoxNewSpaceVdpAddr ($1F1*bytesPerTile)|$4000
.define leftBoxOldSpaceEndTileNum $1A3
.define rightBoxOldSpaceEndTileNum $1B8
.define leftBoxOldSpaceEndVdpAddr ($1A3*bytesPerTile)|$4000
.define rightBoxOldSpaceEndVdpAddr ($1B8*bytesPerTile)|$4000

 
;=====
; user stuff
;=====
  
; base tile at which vwf tiles are initially allocated
.define vwfTileBase_main $000B
; one past the last index of assignable vwf tiles
;.define vwfTileSize_main $007B-vwfTileBase_main
.define vwfTileSize_main $0100-vwfTileBase_main
; if nonzero, assume nametable scroll is zero regardless of actual value
.define vwfScrollZeroFlag_main $00
; high byte for nametable prints
.define vwfNametableHighMask_main $18
  
.define vwfTileBase_fullscreen $000B
; for fullscreen vwf, we assign from tiles $B to $FF with special
; hardcoded exclusions baked into allocVwfTile for the range $7D-$A0,
; which consists of various tiles (box borders, etc.) we want preserved
.define vwfTileSize_fullscreen $00FF-vwfTileBase_fullscreen
.define vwfScrollZeroFlag_fullscreen $00
.define vwfNametableHighMask_fullscreen $18

.define waitBoxMode2 $1F82
.define waitBoxMode1 $1F87

.define maxVwfTiles $100
.define vwfClearTile $1890

.define vwfSpace1pxIndex $1C
.define vwfBorderFillBase $C1
.define opNumIndex $F8
.define opNameIndex $F9
.define opInlineNumIndex $FA
.define vwfBoxClearIndex $FD
.define vwfWaitIndex $F1

.define vwfPeriodIndex $69
.define vwfCommaIndex $6A
.define vwfQuestionMarkIndex $12
.define vwfExclamationMarkIndex $01
.define vwfColonIndex $11
.define vwfSemicolonIndex $68

.define ramBankIdentifier $FE
.define noBankIdentifier $FF
.define newStringSentinel $80

.define oldEnemyDigitOffset $02

;  .define speechEmulationBaseCharCount $01

; multiply by 2 for fast speed, 3 for medium, 4 for slow
.define speechEmulationLen_singlePeriod         8
.define speechEmulationLen_comma                5
.define speechEmulationLen_singleExclaim        8
.define speechEmulationLen_singleQuestion       8
.define speechEmulationLen_ellipsis             10
.define speechEmulationLen_colon                5
.define speechEmulationLen_semicolon            8
;.define speechEmulationLen_interrobang          8

;=====
; vwf settings
;=====

; maximum number of digits that can be printed
.define maxPrintingDigits 5

.define vwfWindowRBorderIndex $1A
.define controlCodeStartIndex $EF
.define controlCodeLimitIndex $FF

.define vwfSpaceOffset $20
.define vwfDigitStartOffset $07
.define vwfDigitSpaceOffset $1F

.define vwfAllocatedSentinel $FF
.define vwfNoDeallocSentinel $FE
.define vwfForceAllocSentinel $FD

;=====
; misc
;=====

.define screenVisibleW 20
.define screenVisibleH 18
.define screenVisibleX 6
.define screenVisibleY 3
;.define screenVisibleX 0
;.define screenVisibleY 0