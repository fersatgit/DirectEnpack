cdrTenthMicron = $00
cdrInch        = $01
cdrFoot        = $02
cdrMillimeter  = $03
cdrCentimeter  = $04
cdrPixel       = $05
cdrMile        = $06
cdrMeter       = $07
cdrKilometer   = $08
cdrDidots      = $09
cdrAgate       = $0A
cdrYard        = $0B
cdrPica        = $0C
cdrCicero      = $0D
cdrPoint       = $0E
cdrUnitQ       = $0F
cdrUnitH       = $10

cdrTopRight    = 1
cdrTopMiddle   = 2
cdrTopLeft     = 3
cdrMiddleLeft  = 4
cdrBottomLeft  = 5
cdrBottomMiddle= 6
cdrBottomRight = 7
cdrMiddleRight = 8
cdrCenter      = 9

VT_EMPTY           = 0;   { [V]   [P]  nothing                     }
VT_NULL            = 1;   { [V]        SQL style Null              }
VT_I2              = 2;   { [V][T][P]  2 byte signed int           }
VT_I4              = 3;   { [V][T][P]  4 byte signed int           }
VT_R4              = 4;   { [V][T][P]  4 byte real                 }
VT_R8              = 5;   { [V][T][P]  8 byte real                 }
VT_CY              = 6;   { [V][T][P]  currency                    }
VT_DATE            = 7;   { [V][T][P]  date                        }
VT_BSTR            = 8;   { [V][T][P]  binary string               }
VT_DISPATCH        = 9;   { [V][T]     IDispatch FAR*              }
VT_ERROR           = 10;  { [V][T]     SCODE                       }
VT_BOOL            = 11;  { [V][T][P]  True=-1, False=0            }
VT_VARIANT         = 12;  { [V][T][P]  VARIANT FAR*                }
VT_UNKNOWN         = 13;  { [V][T]     IUnknown FAR*               }
VT_DECIMAL         = 14;  { [V][T]   [S]  16 byte fixed point      }
VT_I1              = 16;  {    [T]     signed char                 }
VT_UI1             = 17;  {    [T]     unsigned char               }
VT_UI2             = 18;  {    [T]     unsigned short              }
VT_UI4             = 19;  {    [T]     unsigned long               }
VT_I8              = 20;  {    [T][P]  signed 64-bit int           }
VT_UI8             = 21;  {    [T]     unsigned 64-bit int         }
VT_INT             = 22;  {    [T]     signed machine int          }
VT_UINT            = 23;  {    [T]     unsigned machine int        }
VT_VOID            = 24;  {    [T]     C style void                }
VT_HRESULT         = 25;  {    [T]                                 }
VT_PTR             = 26;  {    [T]     pointer type                }
VT_SAFEARRAY       = 27;  {    [T]     (use VT_ARRAY in VARIANT)   }
VT_CARRAY          = 28;  {    [T]     C style array               }
VT_USERDEFINED     = 29;  {    [T]     user defined type          }
VT_LPSTR           = 30;  {    [T][P]  null terminated string      }
VT_LPWSTR          = 31;  {    [T][P]  wide null terminated string }
VT_FILETIME        = 64;  {       [P]  FILETIME                    }
VT_BLOB            = 65;  {       [P]  Length prefixed bytes       }
VT_STREAM          = 66;  {       [P]  Name of the stream follows  }
VT_STORAGE         = 67;  {       [P]  Name of the storage follows }
VT_STREAMED_OBJECT = 68;  {       [P]  Stream contains an object   }
VT_STORED_OBJECT   = 69;  {       [P]  Storage contains an object  }
VT_BLOB_OBJECT     = 70;  {       [P]  Blob contains an object     }
VT_CF              = 71;  {       [P]  Clipboard format            }
VT_CLSID           = 72;  {       [P]  A Class ID                  }
VT_VECTOR          = $1000; {       [P]  simple counted array        }
VT_ARRAY           = $2000; { [V]        SAFEARRAY*                  }
VT_BYREF           = $4000; { [V]                                    }
VT_RESERVED        = $8000;
VT_ILLEGAL         = $ffff;
VT_ILLEGALMASKED   = $0fff;
VT_TYPEMASK        = $0fff;

struct CurveElement
  PositionX   rq 1
  PositionY   rq 1
  ElementType rd 1 ;cdrCurveElementType
  NodeType    rd 1 ;cdrNodeType
  Flags       rb 1
  align       rb 7
ends

struct VARIANT
  type rq 1
  data rq 1
ends

struct DISPPARAMS
  rgvarg            rd 1
  rgdispidNamedArgs rd 1
  cArgs             rd 1
  cNamedArgs        rd 1
ends

struct SAFEARRAYBOUND
  cElements rd 1
  lLbound   rd 1
ends

struct SAFEARRAY
  cDims      rw 1
  fFeatures  rw 1
  cbElements rd 1
  cLocks     rd 1
  pvData     rd 1
  rgsabound  SAFEARRAYBOUND
ends

interface IVGApplication,\
  QueryInterface,\
  AddRef,\
  Release,\
  GetTypeInfoCount,\
  GetTypeInfo,\
  GetIDsOfNames,\
  Invoke,\
  Get_Application,\                        ;: IVGApplication; safecall;
  Get_Parent,\                             ;: IVGApplication; safecall;
  Get_Visible,\                            ;: WordBool; safecall;
  Set_Visible,\                            ;(pVal: WordBool); safecall;
  Get_Documents,\                          ;: IVGDocuments; safecall;
  Get_ActiveDocument,\                     ;: IVGDocument; safecall;
  Get_ActivePage,\                         ;: IVGPage; safecall;
  Get_ActiveWindow,\                       ;: IVGWindow; safecall;
  Get_Windows,\                            ;: IVGWindows; safecall;
  CorelScriptTools,\                       ;: ICorelScriptTools; safecall;
  Get_ActiveWorkspace,\                    ;: IVGWorkspace; safecall;
  Get_Workspaces,\                         ;: IVGWorkspaces; safecall;
  Get_ActivePalette,\                      ;: IVGPalette; safecall;
  Get_Palettes,\                           ;: IVGPalettes; safecall;
  Quit,\                                   ;; safecall;
  CreateColor,\                            ;(const ColorString: WideString): IVGColor; safecall;
  Get_FontList,\                           ;: IVGFontList; safecall;
  Get_AppWindow,\                          ;: IVGAppWindow; safecall;
  Get_RecentFiles,\                        ;: IVGRecentFiles; safecall;
  Get_VBE,\                                ;: IDispatch; safecall;
  cdrMixedDouble,\                         ;: Double; safecall;
  cdrMixedSingle,\                         ;: Single; safecall;
  cdrMixedLong,\                           ;: Integer; safecall;
  Get_EventsEnabled,\                      ;: WordBool; safecall;
  Set_EventsEnabled,\                      ;(pVal: WordBool); safecall;
  OpenDocument,\                           ;(const FileName: WideString; CodePage: Integer): IVGDocument; safecall;
  CreateDocument,\                         ;: IVGDocument; safecall;
  CreateColorEx,\                          ;(ColorModel: Integer; V1: Integer; V2: Integer; V3: Integer; V4: Integer; V5: Integer; V6: Integer; V7: Integer): IVGColor; safecall;
  Get_ArrowHeads,\                         ;: IVGArrowHeads; safecall;
  Get_OutlineStyles,\                      ;: IVGOutlineStyles; safecall;
  Get_Version,\                            ;: WideString; safecall;
  Get_VersionMajor,\                       ;: Integer; safecall;
  Get_VersionMinor,\                       ;: Integer; safecall;
  Get_VersionBuild,\                       ;: Integer; safecall;
  Get_Path,\                               ;: WideString; safecall;
  Get_ConfigPath,\                         ;: WideString; safecall;
  Get_SetupPath,\                          ;: WideString; safecall;
  Get_ActiveLayer,\                        ;: IVGLayer; safecall;
  Get_ActiveSelection,\                    ;: IVGShape; safecall;
  Get_PatternCanvases,\                    ;: IVGPatternCanvases; safecall;
  Get_Clipboard,\                          ;: IVGClipboard; safecall;
  Get_ActiveSelectionRange,\               ;: IVGShapeRange; safecall;
  Get_ActiveTool,\                         ;: cdrTools; safecall;
  Set_ActiveTool,\                         ;(pTool: cdrTools); safecall;
  Get_ActiveShape,\                        ;: IVGShape; safecall;
  Get_Optimization,\                       ;: WordBool; safecall;
  Set_Optimization,\                       ;(pVal: WordBool); safecall;
  Get_PanoseMatching,\                     ;: cdrPanoseMatchingType; safecall;
  Set_PanoseMatching,\                     ;(pVal: cdrPanoseMatchingType); safecall;
  Get_AddIns,\                             ;: IDispatch; safecall;
  CreateRGBColor,\                         ;(Red: Integer; Green: Integer; Blue: Integer): IVGColor; safecall;
  CreateCMYColor,\                         ;(Cyan: Integer; Magenta: Integer; Yellow: Integer): IVGColor; safecall;
  CreateCMYKColor,\                        ;(Cyan: Integer; Magenta: Integer; Yellow: Integer; Black: Integer): IVGColor; safecall;
  CreateGrayColor,\                        ;(GrayValue: Integer): IVGColor; safecall;
  CreateHLSColor,\                         ;(Hue: Integer; Lightness: Integer; Saturation: Integer): IVGColor; safecall;
  CreateHSBColor,\                         ;(Hue: Integer; Saturation: Integer; Brightness: Integer): IVGColor; safecall;
  CreateBWColor,\                          ;(White: WordBool): IVGColor; safecall;
  CreateYIQColor,\                         ;(y: Integer; I: Integer; Q: Integer): IVGColor; safecall;
  CreateLabColor,\                         ;(L: Integer; A: Integer; B: Integer): IVGColor; safecall;
  CreateFixedColor,\                       ;(PaletteID: cdrPaletteID; PaletteIndex: Integer; Tint: Integer): IVGColor; safecall;
  CreateRegistrationColor,\                ;: IVGColor; safecall;
  CreateSnapPoint,\                        ;(PositionX: Double; PositionY: Double): IVGSnapPoint; safecall;
  CreateDocumentFromTemplate,\             ;(const Template: WideString; IncludeGraphics: WordBool): IVGDocument; safecall;
  Get_Printers,\                           ;: IPrnVBAPrinters; safecall;
  Get_PrintJob,\                           ;: IPrnVBAPrintJob; safecall;
  Get_CommandBars,\                        ;: ICUICommandBars; safecall;
  Get_StatusBar,\                          ;: ICUICommandBar; safecall;
  Get_MainMenu,\                           ;: ICUICommandBar; safecall;
  Get_GMSManager,\                         ;: IVGGMSManager; safecall;
  ImportWorkspace,\                        ;(const FileName: WideString); safecall;
  Refresh,\                                ;; safecall;
  CreateStructSaveAsOptions,\              ;: IVGStructSaveAsOptions; safecall;
  CreateStructExportOptions,\              ;: IVGStructExportOptions; safecall;
  CreateStructImportOptions,\              ;: IVGStructImportOptions; safecall;
  CreateStructPaletteOptions,\             ;: IVGStructPaletteOptions; safecall;
  CreateNodeRange,\                        ;: IVGNodeRange; safecall;
  CreateSegmentRange,\                     ;: IVGSegmentRange; safecall;
  CreateShapeRange,\                       ;: IVGShapeRange; safecall;
  CreatePatternCanvas,\                    ;: IVGPatternCanvas; safecall;
  CreateCurve,\                            ;(const Document: IVGDocument): IVGCurve; safecall;
  Get_UserDataPath,\                       ;: WideString; safecall;
  InitializeVBA,\                          ;: WordBool; safecall;
  Get_HelpFile,\                           ;: WideString; safecall;
  Get_FrameWork,\                          ;: ICUIFrameWork; safecall;
  CreateStructFontProperties,\             ;: IVGStructFontProperties; safecall;
  CreateStructAlignProperties,\            ;: IVGStructAlignProperties; safecall;
  CreateStructSpaceProperties,\            ;: IVGStructSpaceProperties; safecall;
  CreateStructHyphenationSettings,\        ;: IVGStructHyphenationSettings; safecall;
  Get_Components,\                         ;: IVGComponents; safecall;
  Get_SymbolLibraries,\                    ;: IVGSymbolLibraries; safecall;
  AdviseEvents,\                           ;(const EventSink: IDispatch): Integer; safecall;
  UnadviseEvents,\                         ;(Cookie: Integer); safecall;
  Get_ID,\                                 ;: cdrApplicationID; safecall;
  Get_Name,\                               ;: WideString; safecall;
  Get_Class,\                              ;: cdrApplicationClass; safecall;
  Get_PlatformVersionMajor,\               ;: Integer; safecall;
  Get_PlatformVersionMinor,\               ;: Integer; safecall;
  CheckPlatformVersion,\                   ;(VersionMajor: Integer; VersionMinor: Integer): Integer; safecall;
  Get_Status,\                             ;: IVGAppStatus; safecall;
  ConvertUnits,\                           ;(Value: Double; FromUnit: cdrUnit; ToUnit: cdrUnit): Double; safecall;
  Get_UILanguage,\                         ;: cdrTextLanguage; safecall;
  IsUILanguageAvailable,\                  ;(Language: cdrTextLanguage): WordBool; safecall;
  Get_PageSizes,\                          ;: IVGPageSizes; safecall;
  Get_Unit:,\                              ; cdrUnit; safecall;
  Set_Unit,\                               ;(pVal: cdrUnit); safecall;
  ConvertToUnicode,\                       ;(const String_: WideString; CodePage: Integer): WideString; safecall;
  ConvertFromUnicode,\                     ;(const String_: WideString; CodePage: Integer): WideString; safecall;
  Get_UserWorkspacePath,\                  ;: WideString; safecall;
  Get_LanguagePath,\                       ;: WideString; safecall;
  Get_ActiveTreeManager,\                  ;: IVGTreeManager; safecall;
  Get_ActiveVirtualLayer,\                 ;: IVGLayer; safecall;
  CreateDuotone,\                          ;: IVGDuotone; safecall;
  Get_ColorManager,\                       ;: IVGColorManager; safecall;
  Get_EnhancedOutlines,\                   ;: IVGOutlineStyles; safecall;
  AddPluginCommand,\                       ;(const CommandID: WideString; const Caption: WideString; const Tooltip: WideString): WordBool; safecall;
  RemovePluginCommand,\                    ;(const CommandID: WideString): WordBool; safecall;
  CreateOutlineStyle,\                     ;(DashDotCount: Integer; var DashDotLengths: PSafeArray): IVGOutlineStyle; safecall;
  Get_StartupMode,\                        ;: cdrAppStartupMode; safecall;
  Set_StartupMode,\                        ;(pVal: cdrAppStartupMode); safecall;
  Get_GlobalUserData,\                     ;: IVGProperties; safecall;
  Get_SessionUserData,\                    ;: IVGProperties; safecall;
  Evaluate,\                               ;(const Expression: WideString): OleVariant; safecall;
  CreateRect,\                             ;(x: Double; y: Double; Width: Double; Height: Double): IVGRect; safecall;
  ForceUpdateFontTable,\                   ;; safecall;
  Get_ActiveSpread,\                       ;: IVGSpread; safecall;
  OpenDocumentAsCopy,\                     ;(const FileName: WideString; const Options: IVGStructOpenOptions): IVGDocument; safecall;
  Get_DefaultColorContext,\                ;: IVGColorContext; safecall;
  CreateColorContext,\                     ;(const RGBProfile: IVGColorProfile; const CMYKProfile: IVGColorProfile; const GrayscaleProfile: IVGColorProfile; RenderingIntent: clrRenderingIntent; BlendingColorModel: clrColorModel): IVGColorContext; safecall;
  CreateColorContext2,\                    ;(const ColorProfileList: WideString; RenderingIntent: clrRenderingIntent; BlendingColorModel: clrColorModel): IVGColorContext; safecall;
  CreateDocumentEx,\                       ;(const Options: IVGStructCreateOptions): IVGDocument; safecall;
  OpenDocumentEx,\                         ;(const FileName: WideString; const Options: IVGStructOpenOptions): IVGDocument; safecall;
  CreateStructOpenOptions,\                ;: IVGStructOpenOptions; safecall;
  CreateStructCreateOptions,\              ;: IVGStructCreateOptions; safecall;
  CreateStructPasteOptions,\               ;: IVGStructPasteOptions; safecall;
  CreateProofColorSettings,\               ;(const ProfileName: WideString; RenderingIntent: clrRenderingIntent; PreserveColorValues: WordBool): IVGProofColorSettings; safecall;
  Get_PaletteManager,\                     ;: IVGPaletteManager; safecall;
  CreateSpotColor,\                        ;(const PaletteIdentifier: WideString; SpotColorID: Integer; Tint: Integer): IVGColor; safecall;
  CreateSpotColorByName,\                  ;(const PaletteIdentifier: WideString; const SpotColorName: WideString; Tint: Integer): IVGColor; safecall;
  CreatePaletteColor,\                     ;(const PaletteIdentifier: WideString; ColorIndex: Integer): IVGColor; safecall;
  GetSupportedOpenTypeFeatures,\           ;: PSafeArray; safecall;
  CreateFillMetadata,\                     ;: IVGFillMetadata; safecall;
  Get_AddonPath,\                          ;: WideString; safecall;
  Get_ProgramPath,\                        ;: WideString; safecall;
  Get_ActiveToolStateGuid,\                ;: WideString; safecall;
  Set_ActiveToolStateGuid,\                ;(const pTool: WideString); safecall;
  RegisterToolState,\                      ;(const ToolStateGuid: WideString; const ToolStateName: WideString; const ToolState: IVGToolState); safecall;
  UnregisterToolState,\                    ;(const ToolStateGuid: WideString): WordBool; safecall;
  CreateOnScreenCurve,\                    ;: IVGOnScreenCurve; safecall;
  CreateOnScreenHandle,\                   ;: IVGOnScreenHandle; safecall;
  CreateOnScreenText,\                     ;: IVGOnScreenText; safecall;
  Get_Math,\                               ;: IVGMathUtils; safecall;
  RegisterUserApplicationPreference,\      ;(const GroupName: WideString; const KeyName: WideString; DefaultVal: OleVariant); safecall;
  GetApplicationPreferenceValue,\          ;(const GroupName: WideString; const KeyName: WideString): OleVariant; safecall;
  SetApplicationPreferenceValue,\          ;(const GroupName: WideString; const KeyName: WideString; newVal: OleVariant); safecall;
  CreateProperties,\                       ;: IVGProperties; safecall;
  RegisterToolShape,\                      ;(const ToolShapeGuid: WideString; const ToolShapeAttributes: IVGToolShapeAttributes; const ToolShape: IVGToolShape); safecall;
  CreateToolShapeAttributes,\              ;: IVGToolShapeAttributes; safecall;
  Get_UILanguageCode,\                     ;: WideString; safecall;
  StartTemporaryToolState                  ;(const StateGuid: WideString); safecall;

interface IVGSubPaths,\
  QueryInterface,\
  AddRef,\
  Release,\
  GetTypeInfoCount,\
  GetTypeInfo,\
  GetIDsOfNames,\
  Invoke,\
  Get_Application,\    ;: IVGApplication; safecall;
  Get_Parent,\         ;: IVGCurve; safecall;
  Get_Item,\           ;(Index: Integer): IVGSubPath; safecall;
  Get__NewEnum,\       ;: IUnknown; safecall;
  Get_Count,\          ;: Integer; safecall;
  Get_First,\          ;: IVGSubPath; safecall;
  Get_Last             ;: IVGSubPath; safecall;

interface IVGCurve,\
  QueryInterface,\
  AddRef,\
  Release,\
  GetTypeInfoCount,\
  GetTypeInfo,\
  GetIDsOfNames,\
  Invoke,\
  Get_Length,\                                   ;: Double; safecall;
  Get_SubPaths,\                                 ;: IVGSubPaths; safecall;
  Get_Nodes,\                                    ;: IVGNodes; safecall;
  Get_Segments,\                                 ;: IVGSegments; safecall;
  Get_Closed,\                                   ;: WordBool; safecall;
  Set_Closed,\                                   ;(pVal: WordBool); safecall;
  Selection,\                                    ;: IVGNodeRange; safecall;
  CreateSubPath,\                                ;(x: Double; y: Double): IVGSubPath; safecall;
  ReverseDirection,\                             ;; safecall;
  IsOnCurve,\                                    ;(x: Double; y: Double; HotArea: Double): cdrPositionOfPointOverShape; safecall;
  BindToDocument,\                               ;(const Document: IVGDocument); safecall;
  GetCopy,\                                      ;: IVGCurve; safecall;
  CopyAssign,\                                   ;(const Source: IVGCurve); safecall;
  CreateSubPathFromArray,\                       ;(var Source: PSafeArray; Closed: WordBool; NumElements: Integer): IVGSubPath; safecall;
  AppendCurve,\                                  ;(const Source: IVGCurve); safecall;
  GetCurveInfo,\                                 ;: PSafeArray; safecall;
  PutCurveInfo,\                                 ;(var Source: PSafeArray; NumElements: Integer): Integer; safecall;
  ClearSelection,\                               ;; safecall;
  GetPolyline,\                                  ;(CurvePrecision: Integer): IVGCurve; safecall;
  RemoveOverlaps,\                               ;: IVGCurve; safecall;
  Contour,\                                      ;(Offset: Double; Direction: cdrContourDirection; EndCapType: cdrContourEndCapType; CornerType: cdrContourCornerType; MiterLimit: Double): IVGCurve; safecall;
  IsPointInside,\                                ;(x: Double; y: Double): WordBool; safecall;
  IsRectOnEdge,\                                 ;(x1: Double; y1: Double; x2: Double; y2: Double): WordBool; safecall;
  FindClosestSegment,\                           ;(x: Double; y: Double; out ParamOffset: Double): IVGSegment; safecall;
  FindNodeAtPoint,\                              ;(x: Double; y: Double; HotArea: Double): IVGNode; safecall;
  FindSegmentAtPoint,\                           ;(x: Double; y: Double; out ParamOffset: Double; HotArea: Double): IVGSegment; safecall;
  WeldWith,\                                     ;(const Curve: IVGCurve): IVGCurve; safecall;
  Get_IsClockwise,\                              ;: WordBool; safecall;
  Get_Area,\                                     ;: Double; safecall;
  GetBoundingBox,\                               ;(out x: Double; out y: Double; out Width: Double; out Height: Double): WordBool; safecall;
  Get_BoundingBox,\                              ;: IVGRect; safecall;
  IntersectsWith,\                               ;(const Curve: IVGCurve): WordBool; safecall;
  AppendSubpathFitToPoints,\                     ;(const Points: IVGPointRange; UseCurrentViewForTolerance: WordBool; tolerance: Double); safecall;
  AppendSubpathFitToPointsAndCusps,\             ;(const Points: IVGPointRange; var CuspIndexArray: PSafeArray; UseCurrentViewForTolerance: WordBool; tolerance: Double); safecall;
  ApplyTransformMatrix,\                         ;(const TransformMatrix: IVGTransformMatrix); safecall;
  AppendSubpathFromPoints,\                      ;(const Points: IVGPointRange; Close: WordBool); safecall;
  CreateCurveMappedToStroke,\                    ;(const Stroke: IVGSubPath; ScaleY: Double; SelfWeld: WordBool): IVGCurve; safecall;
  CreateCurveMappedToStrokeAndReferenceLine,\    ;(const Stroke: IVGSubPath; const Start: IVGPoint; const End_: IVGPoint; ScaleY: Double; SelfWeld: WordBool): IVGCurve; safecall;
  AutoReduceNodes,\                              ;(AmountToReduce0To100: Double; SelectedNodesOnly: WordBool); safecall;
  JoinTouchingSubpaths,\                         ;(AllowSubpathReversals: WordBool; tolerance: Double); safecall;
  AppendSubpathCircle,\                          ;(CenterX: Double; CenterY: Double; Radius: Double); safecall;
  AppendSubpathRectangle,\                       ;(Left: Double; Top: Double; Right: Double; Bottom: Double); safecall;
  AppendSubpathThreePointArc,\                   ;(StartX: Double; StartY: Double; EndX: Double; EndY: Double; ThirdX: Double; ThirdY: Double); safecall;
  SelfWeldClosedSubpaths,\                       ;; safecall;
  AppendSubpathEllipse,\                         ;(CenterX: Double; CenterY: Double; RadiusH: Double; RadiusV: Double); safecall;
  WeldEx                                         ;(const TargetCurve: IVGCurve; Method: cdrWeldMethod; WindingSource: WordBool; WindingTarget: WordBool; Flags: Integer): IVGCurve; safecall;

interface IVGShape,\
  QueryInterface,\
  AddRef,\
  Release,\
  GetTypeInfoCount,\
  GetTypeInfo,\
  GetIDsOfNames,\
  Invoke,\
  Get_Application,\                    ;: IVGApplication; safecall;
  Get_Parent,\                         ;: IDispatch; safecall;
  Get_StaticID,\                       ;: Integer; safecall;
  ConvertToCurves,\                    ;; safecall;
  Get_Name,\                           ;: WideString; safecall;
  Set_Name,\                           ;(const pVal: WideString); safecall;
  Get_Shapes,\                         ;: IVGShapes; safecall;
  Get_Rectangle,\                      ;: IVGRectangle; safecall;
  Get_PositionX,\                      ;: Double; safecall;
  Set_PositionX,\                      ;(pVal: Double); safecall;
  Get_PositionY,\                      ;: Double; safecall;
  Set_PositionY,\                      ;(pVal: Double); safecall;
  Get_SizeWidth,\                      ;: Double; safecall;
  Set_SizeWidth,\                      ;(pVal: Double); safecall;
  Get_SizeHeight,\                     ;: Double; safecall;
  Set_SizeHeight,\                     ;(pVal: Double); safecall;
  Get_Ellipse,\                        ;: IVGEllipse; safecall;
  Get_Polygon,\                        ;: IVGPolygon; safecall;
  Get_Curve,\                          ;: IVGCurve; safecall;
  Get_Bitmap,\                         ;: IVGBitmap; safecall;
  Get_type,\                           ;: cdrShapeType; safecall;
  Get_Outline,\                        ;: IVGOutline; safecall;
  Get_Fill,\                           ;: IVGFill; safecall;
  Get_Text,\                           ;: IVGText; safecall;
  Delete,\                             ;; safecall;
  Duplicate,\                          ;(OffsetX: Double; OffsetY: Double): IVGShape; safecall;
  Skew,\                               ;(AngleX: Double; AngleY: Double); safecall;
  Move,\                               ;(DeltaX: Double; DeltaY: Double); safecall;
  Get_RotationAngle,\                  ;: Double; safecall;
  Set_RotationAngle,\                  ;(pVal: Double); safecall;
  Get_RotationCenterX,\                ;: Double; safecall;
  Set_RotationCenterX,\                ;(pVal: Double); safecall;
  Get_RotationCenterY,\                ;: Double; safecall;
  Set_RotationCenterY,\                ;(pVal: Double); safecall;
  Rotate,\                             ;(Angle: Double); safecall;
  ConvertToBitmap,\                    ;(BitDepth: Integer; Grayscale: WordBool; Dithered: WordBool; TransparentBG: WordBool; Resolution: Integer; AntiAliasing: cdrAntiAliasingType; UseColorProfile: WordBool; MultiChannel: WordBool; AlwaysOverprintBlack: WordBool; OverprintBlackLimit: Integer): IVGShape; safecall;
  Group,\                              ;: IVGShape; safecall;
  Ungroup,\                            ;; safecall;
  UngroupAll,\                         ;; safecall;
  OrderToFront,\                       ;; safecall;
  OrderToBack,\                        ;; safecall;
  OrderForwardOne,\                    ;; safecall;
  OrderBackOne,\                       ;; safecall;
  OrderFrontOf,\                       ;(const Shape: IVGShape); safecall;
  OrderBackOf,\                        ;(const Shape: IVGShape); safecall;
  OrderIsInFrontOf,\                   ;(const Shape: IVGShape): WordBool; safecall;
  AddToSelection,\                     ;; safecall;
  RemoveFromSelection,\                ;; safecall;
  Separate,\                           ;; safecall;
  Get_Layer,\                          ;: IVGLayer; safecall;
  Set_Layer,\                          ;(const ppVal: IVGLayer); safecall;
  Get_SnapPoints,\                     ;: IVGSnapPoints; safecall;
  Get_Connector,\                      ;: IVGConnector; safecall;
  IsOnShape,\                          ;(x: Double; y: Double; HotArea: Double): cdrPositionOfPointOverShape; safecall;
  CreateArrowHead,\                    ;: IVGArrowHead; safecall;
  Copy,\                               ;; safecall;
  Cut,\                                ;; safecall;
  Clone,\                              ;(OffsetX: Double; OffsetY: Double): IVGShape; safecall;
  Stretch,\                            ;(StretchX: Double; StretchY: Double; StretchCharactersSize: WordBool); safecall;
  SetPosition,\                        ;(PositionX: Double; PositionY: Double); safecall;
  SetSize,\                            ;(Width: Double; Height: Double); safecall;
  GetPosition,\                        ;(out PositionX: Double; out PositionY: Double); safecall;
  GetSize,\                            ;(out Width: Double; out Height: Double); safecall;
  Get_Properties,\                     ;: IVGProperties; safecall;
  OrderReverse,\                       ;; safecall;
  Combine,\                            ;: IVGShape; safecall;
  BreakApart,\                         ;; safecall;
  Set_Fill,\                           ;(const ppVal: IVGFill); safecall;
  Weld,\                               ;(const TargetShape: IVGShape; LeaveSource: WordBool; LeaveTarget: WordBool): IVGShape; safecall;
  Trim,\                               ;(const TargetShape: IVGShape; LeaveSource: WordBool; LeaveTarget: WordBool): IVGShape; safecall;
  Intersect,\                          ;(const TargetShape: IVGShape; LeaveSource: WordBool; LeaveTarget: WordBool): IVGShape; safecall;
  Get_Effects,\                        ;: IVGEffects; safecall;
  Get_Effect,\                         ;: IVGEffect; safecall;
  CreateDropShadow,\                   ;(Type_: cdrDropShadowType; Opacity: Integer; Feather: Integer; OffsetX: Double; OffsetY: Double; const Color: IVGColor; FeatherType: cdrFeatherType; FeatherEdge: cdrEdgeType; PerspectiveAngle: Double; PerspectiveStretch: Double; Fade: Integer; MergeMode: cdrMergeMode): IVGEffect; safecall;
  CreateBlend,\                        ;(const Shape: IVGShape; Steps: integer; ColorBlendType: cdrFountainFillBlendType; Mode: cdrBlendMode; Spacing: Double; Angle: Double; Loop: WordBool; const Path: IVGShape; RotateShapes: WordBool; SpacingAccel: Integer; ColorAccel: Integer; AccelSize: WordBool): IVGEffect; safecall;
  CreateExtrude,\                      ;(Type_: cdrExtrudeType; VPType: cdrExtrudeVPType; VPX: Double; VPY: Double; Depth: Double; Shading: cdrExtrudeShading; const BaseColor: IVGColor; const ShadingColor: IVGColor; BevelDepth: Double; BevelAngle: Double; const BevelColor: IVGColor; BevelOnly: WordBool): IVGEffect; safecall;
  CreateEnvelope,\                     ;(PresetIndex: Integer; Mode: cdrEnvelopeMode; KeepLines: WordBool): IVGEffect; safecall;
  Flip,\                               ;(Axes: cdrFlipAxes); safecall;
  Get_Locked,\                         ;: WordBool; safecall;
  Set_Locked,\                         ;(pVal: WordBool); safecall;
  Get_OriginalWidth,\                  ;: Double; safecall;
  Get_OriginalHeight,\                 ;: Double; safecall;
  Get_Selected,\                       ;: WordBool; safecall;
  Set_Selected,\                       ;(pVal: WordBool); safecall;
  CreateLens,\                         ;(Type_: cdrLensType; RateOrMagnification: Double; const Color1: IVGColor; const Color2: IVGColor; ColorMapPalette: cdrFountainFillBlendType): IVGEffect; safecall;
  CreatePerspective,\                  ;(HorizVanishPointX: OleVariant; HorizVanishPointY: OleVariant; VertVanishPointX: OleVariant; VertVanishPointY: OleVariant): IVGEffect; safecall;
  CreateContour,\                      ;(Direction: cdrContourDirection; Offset: Double; Steps: Integer; BlendType: cdrFountainFillBlendType; const OutlineColor: IVGColor; const FillColor: IVGColor; const FillColor2: IVGColor; SpacingAccel: Integer; ColorAccel: Integer; EndCapType: cdrContourEndCapType; CornerType: cdrContourCornerType; MiterLimit: Double): IVGEffect; safecall;
  CreatePushPullDistortion,\           ;(OriginX: Double; OriginY: Double; Amplitude: Integer): IVGEffect; safecall;
  CreateZipperDistortion,\             ;(OriginX: Double; OriginY: Double; Amplitude: Integer; Frequency: Integer; Random: WordBool; Smooth: WordBool; Local: WordBool): IVGEffect; safecall;
  CreateTwisterDistortion,\            ;(OriginX: Double; OriginY: Double; Angle: Double): IVGEffect; safecall;
  Get_Guide,\                          ;: IVGGuide; safecall;
  AddToPowerClip,\                     ;(const Shape: IVGShape; CenterInContainer: cdrTriState); safecall;
  RemoveFromContainer,\                ;(Level: Integer); safecall;
  Get_PowerClip,\                      ;: IVGPowerClip; safecall;
  Get_PowerClipParent,\                ;: IVGShape; safecall;
  Get_DrapeFill,\                      ;: WordBool; safecall;
  Set_DrapeFill,\                      ;(pVal: WordBool); safecall;
  Get_OverprintFill,\                  ;: WordBool; safecall;
  Set_OverprintFill,\                  ;(pVal: WordBool); safecall;
  Get_OverprintOutline,\               ;: WordBool; safecall;
  Set_OverprintOutline,\               ;(pVal: WordBool); safecall;
  Get_URL,\                            ;: IVGURL; safecall;
  Get_ObjectData,\                     ;: IVGDataItems; safecall;
  Get_CloneLink,\                      ;: IVGCloneLink; safecall;
  Get_Clones,\                         ;: IVGShapeRange; safecall;
  Get_AbsoluteHScale,\                 ;: Double; safecall;
  Get_AbsoluteVScale,\                 ;: Double; safecall;
  Get_AbsoluteSkew,\                   ;: Double; safecall;
  Get_Transparency,\                   ;: IVGTransparency; safecall;
  GetMatrix,\                          ;(out d11: Double; out d12: Double; out d21: Double; out d22: Double; out tx: Double; out ty: Double); safecall;
  SetMatrix,\                          ;(d11: Double; d12: Double; d21: Double; d22: Double; tx: Double; ty: Double); safecall;
  ConvertToBitmapEx,\                  ;(Mode: cdrImageType; Dithered: WordBool; Transparent: WordBool; Resolution: Integer; AntiAliasing: cdrAntiAliasingType; UseColorProfile: WordBool; AlwaysOverprintBlack: WordBool; OverprintBlackLimit: Integer): IVGShape; safecall;
  SkewEx,\                             ;(AngleX: Double; AngleY: Double; CenterX: Double; CenterY: Double); safecall;
  RotateEx,\                           ;(Angle: Double; CenterX: Double; CenterY: Double); safecall;
  Get_ParentGroup,\                    ;: IVGShape; safecall;
  SetBoundingBox,\                     ;(x: Double; y: Double; Width: Double; Height: Double; KeepAspect: WordBool; ReferencePoint: cdrReferencePoint); safecall;
  CreateSelection,\                    ;; safecall;
  SetRotationCenter,\                  ;(x: Double; y: Double); safecall;
  ClearEffect,\                        ;(Type_: cdrEffectType); safecall;
  Get_Next,\                           ;(Level: cdrShapeLevel; EnterGroups: WordBool; Loop: WordBool): IVGShape; safecall;
  Get_Previous,\                       ;(Level: cdrShapeLevel; EnterGroups: WordBool; Loop: WordBool): IVGShape; safecall;
  StretchEx,\                          ;(CenterX: Double; CenterY: Double; StretchX: Double; StretchY: Double; StretchCharactersSize: WordBool); safecall;
  SetSizeEx,\                          ;(CenterX: Double; CenterY: Double; Width: Double; Height: Double); safecall;
  GetBoundingBox,\                     ;(out x: Double; out y: Double; out Width: Double; out Height: Double; UseOutline: WordBool); safecall;
  UngroupEx,\                          ;: IVGShapeRange; safecall;
  UngroupAllEx,\                       ;: IVGShapeRange; safecall;
  BreakApartEx,\                       ;: IVGShapeRange; safecall;
  ApplyStyle,\                         ;(const StyleName: WideString); safecall;
  Get_WrapText,\                       ;: cdrWrapStyle; safecall;
  Set_WrapText,\                       ;(pVal: cdrWrapStyle); safecall;
  Get_TextWrapOffset,\                 ;: Double; safecall;
  Set_TextWrapOffset,\                 ;(pVal: Double); safecall;
  PlaceTextInside,\                    ;(const TextShape: IVGShape): IVGShape; safecall;
  Get_DisplayCurve,\                   ;: IVGCurve; safecall;
  CustomCommand,\                      ;(const ComponentID: WideString; const CommandID: WideString; var Parameters: PSafeArray): OleVariant; safecall;
  Get_Custom,\                         ;: IVGCustomShape; safecall;
  CreateCustomEffect,\                 ;(const EffectID: WideString; var Parameters: PSafeArray): IVGEffect; safecall;
  CreateCustomDistortion,\             ;(const DistortionID: WideString; var Parameters: PSafeArray): IVGEffect; safecall;
  AlignToShape,\                       ;(Type_: cdrAlignType; const Shape: IVGShape; TextAlignOrigin: cdrTextAlignOrigin); safecall;
  AlignToShapeRange,\                  ;(Type_: cdrAlignType; const ShapeRange: IVGShapeRange; TextAlignOrigin: cdrTextAlignOrigin); safecall;
  AlignToPage,\                        ;(Type_: cdrAlignType; TextAlignOrigin: cdrTextAlignOrigin); safecall;
  AlignToPageCenter,\                  ;(Type_: cdrAlignType; TextAlignOrigin: cdrTextAlignOrigin); safecall;
  AlignToGrid,\                        ;(Type_: cdrAlignType; TextAlignOrigin: cdrTextAlignOrigin); safecall;
  AlignToPoint,\                       ;(Type_: cdrAlignType; x: Double; y: Double; TextAlignOrigin: cdrTextAlignOrigin); safecall;
  Get_Dimension,\                      ;: IVGDimension; safecall;
  Get_Symbol,\                         ;: IVGSymbol; safecall;
  ConvertToSymbol,\                    ;(const Name: WideString): IVGShape; safecall;
  Get_OLE,\                            ;: IVGOLE; safecall;
  DuplicateAsRange,\                   ;(OffsetX: Double; OffsetY: Double): IVGShapeRange; safecall;
  CloneAsRange,\                       ;(OffsetX: Double; OffsetY: Double): IVGShapeRange; safecall;
  MoveToLayer,\                        ;(const Layer: IVGLayer); safecall;
  CopyToLayer,\                        ;(const Layer: IVGLayer): IVGShape; safecall;
  CopyToLayerAsRange,\                 ;(const Layer: IVGLayer): IVGShapeRange; safecall;
  ClearTransformations,\               ;; safecall;
  Distribute,\                         ;(Type_: cdrDistributeType; PageExtent: WordBool); safecall;
  CompareTo,\                          ;(const Shape: IVGShape; CompareType: cdrCompareType; Condition: cdrCompareCondition): WordBool; safecall;
  Get_Selectable,\                     ;: WordBool; safecall;
  ApplyEffectInvert,\                  ;; safecall;
  ApplyEffectPosterize,\               ;(Level: Integer); safecall;
  ApplyEffectBCI,\                     ;(Brighness: Integer; Contrast: Integer; Intensity: Integer); safecall;
  ApplyEffectColorBalance,\            ;(CyanRed: Integer; MagentaGreen: Integer; YellowBlue: Integer; ApplyToShadows: WordBool; ApplyToMidtones: WordBool; ApplyToHighlights: WordBool; PreserveLuminance: WordBool); safecall;
  ApplyEffectGamma,\                   ;(Gamma: Double); safecall;
  ApplyEffectHSL,\                     ;(Hue: OleVariant; Saturation: OleVariant; Lightness: OleVariant); safecall;
  TransformMatrix,\                    ;(d11: Double; d12: Double; d21: Double; d22: Double; tx: Double; ty: Double); safecall;
  AffineTransform,\                    ;(d11: Double; d12: Double; d21: Double; d22: Double; CenterX: Double; CenterY: Double); safecall;
  Get_TreeNode,\                       ;: IVGTreeNode; safecall;
  ReplaceWith,\                        ;(const VirtualShape: IVGShape); safecall;
  Get_Virtual,\                        ;: WordBool; safecall;
  Get_CanHaveFill,\                    ;: WordBool; safecall;
  Get_CanHaveOutline,\                 ;: WordBool; safecall;
  Get_IsSimpleShape,\                  ;: WordBool; safecall;
  Fillet,\                             ;(Radius: Double; CombineSmoothSegments: WordBool); safecall;
  Chamfer,\                            ;(DistanceA: Double; DistanceB: Double; CombineSmoothSegments: WordBool); safecall;
  Scallop,\                            ;(Radius: Double; CombineSmoothSegments: WordBool); safecall;
  Get_FillMode,\                       ;: cdrFillMode; safecall;
  Set_FillMode,\                       ;(pVal: cdrFillMode); safecall;
  Get_LeftX,\                          ;: Double; safecall;
  Get_RightX,\                         ;: Double; safecall;
  Get_TopY,\                           ;: Double; safecall;
  Get_BottomY,\                        ;: Double; safecall;
  StepAndRepeat,\                      ;(NumCopies: Integer; DistanceX: Double; DistanceY: Double; ModeX: cdrDistanceMode; DirectionX: cdrDirection; ModeY: cdrDistanceMode; DirectionY: cdrDirection): IVGShapeRange; safecall;
  Get_OverprintBitmap,\                ;: WordBool; safecall;
  Set_OverprintBitmap,\                ;(pVal: WordBool); safecall;
  IsTypeAnyOf,\                        ;(var TypeList: PSafeArray): WordBool; safecall;
  GetLinkedShapes,\                    ;(LinkType: cdrShapeLinkType): IVGShapeRange; safecall;
  CreateEnvelopeFromShape,\            ;(const Source: IVGShape; Mode: cdrEnvelopeMode; KeepLines: WordBool; CopyMode: cdrEnvelopeCopyMode; CornerIndices: OleVariant): IVGEffect; safecall;
  CreateEnvelopeFromCurve,\            ;(const Source: IVGCurve; Mode: cdrEnvelopeMode; KeepLines: WordBool; CopyMode: cdrEnvelopeCopyMode; CornerIndices: OleVariant): IVGEffect; safecall;
  Get_EPS,\                            ;: IVGEPS; safecall;
  Evaluate,\                           ;(const Expression: WideString): OleVariant; safecall;
  Get_BoundingBox,\                    ;: IVGRect; safecall;
  GetPositionEx,\                      ;(ReferencePoint: cdrReferencePoint; out x: Double; out y: Double); safecall;
  SetPositionEx,\                      ;(ReferencePoint: cdrReferencePoint; x: Double; y: Double); safecall;
  Get_CenterX,\                        ;: Double; safecall;
  Set_CenterX,\                        ;(pVal: Double); safecall;
  Get_CenterY,\                        ;: Double; safecall;
  Set_CenterY,\                        ;(pVal: Double); safecall;
  Set_LeftX,\                          ;(pVal: Double); safecall;
  Set_RightX,\                         ;(pVal: Double); safecall;
  Set_TopY,\                           ;(pVal: Double); safecall;
  Set_BottomY,\                        ;(pVal: Double); safecall;
  Get_ZOrder,\                         ;: Integer; safecall;
  CompareToEx,\                        ;(const Shape2: IVGShape; const Condition: WideString; Data: OleVariant): WordBool; safecall;
  CopyPropertiesFrom,\                 ;(const Source: IVGShape; Properties: cdrCopyProperties): WordBool; safecall;
  GetOverprintFillState,\              ;: cdrOverprintState; safecall;
  GetOverprintOutlineState,\           ;: cdrOverprintState; safecall;
  Get_Page,\                           ;: IVGPage; safecall;
  SnapPointsOfType,\                   ;;(TypeSet: cdrPointType): IVGSnapPoints; safecall;
  FindSnapPoint,\                      ;(const ReferenceData: WideString): IVGSnapPoint; safecall;
  Get_Spread,\                         ;: IVGSpread; safecall;
  Get_PixelAlignedRendering,\          ;: WordBool; safecall;
  Set_PixelAlignedRendering,\          ;(pVal: WordBool); safecall;
  Get_BSpline,\                        ;: IVGBSpline; safecall;
  CreateDocumentFrom,\                 ;(TemporaryDocument: WordBool): IVGDocument; safecall;
  AlignAndDistribute,\                 ;(MethodH: cdrAlignDistributeH; MethodV: cdrAlignDistributeV; AlignTo: cdrAlignShapesTo; DistributeArea: cdrDistributeArea; UseOutline: WordBool; TextAlignOrigin: cdrTextAlignOrigin; PointX: Double; PointY: Double; const DistributeRect: IVGRect); safecall;
  Get_Style,\                          ;: IVGStyle; safecall;
  CreateBoundary,\                     ;(x: Double; y: Double; PlaceOnTop: WordBool; DeleteSource: WordBool): IVGShape; safecall;
  EqualDivide,\                        ;(Divisions: Integer; Gap: Double; Group: WordBool; Combine: WordBool; DeleteSource: WordBool): IVGShapeRange; safecall;
  Project,\                            ;(Plane: cdrProjectPlane; ReferencePoint: cdrReferencePoint; ApplyToDuplicate: WordBool): IVGShape; safecall;
  Unproject,\                          ;(Plane: cdrProjectPlane; ReferencePoint: cdrReferencePoint; ApplyToDuplicate: WordBool): IVGShape; safecall;
  Get_TransformationMatrix,\           ;: IVGTransformMatrix; safecall;
  Set_TransformationMatrix,\           ;(const TransformMatrix: IVGTransformMatrix); safecall;
  ApplyTransformMatrix,\               ;(const TransformMatrix: IVGTransformMatrix); safecall;
  Get_Visible,\                        ;: WordBool; safecall;
  Set_Visible,\                        ;(pVal: WordBool); safecall;
  ModifyToolShapeProperties,\          ;(const ShapePropertiesToModify: IVGProperties); safecall;
  GetToolShapeGuid,\                   ;: WideString; safecall;
  CreateParallelCurves,\               ;(Count: Integer; distanceBetweenCurves: Double): IVGShapeRange; safecall;
  FindShapeAtPoint                     ;(x: Double; y: Double; TreatAsFilled: WordBool): IVGShape; safecall;

interface IVGShapeRange,\
  QueryInterface,\
  AddRef,\
  Release,\
  GetTypeInfoCount,\
  GetTypeInfo,\
  GetIDsOfNames,\
  Invoke,\
  Get_Application,\                  ;: IVGApplication; safecall;
  Get_Parent,\                       ;: IVGApplication; safecall;
  Get_Item,\                         ;(IndexOrName: OleVariant): IVGShape; safecall;
  Get__NewEnum,\                     ;: IUnknown; safecall;
  Get_Count,\                        ;: Integer; safecall;
  Add,\                              ;(const Shape: IVGShape); safecall;
  Remove,\                           ;(Index: Integer); safecall;
  IndexOf,\                          ;(const Shape: IVGShape): Integer; safecall;
  AddToSelection,\                   ;; safecall;
  ConvertToCurves,\                  ;; safecall;
  ConvertToBitmap,\                  ;(BitDepth: Integer; Grayscale: WordBool; Dithered: WordBool; TransparentBG: WordBool; Resolution: Integer; AntiAliasing: cdrAntiAliasingType; UseColorProfile: WordBool; MultiChannel: WordBool; AlwaysOverprintBlack: WordBool; OverprintBlackLimit: Integer): IVGShape; safecall;
  Copy,\                             ;; safecall;
  Cut,\                              ;; safecall;
  Delete,\                           ;; safecall;
  GetPosition,\                      ;(out PositionX: Double; out PositionY: Double); safecall;
  GetSize,\                          ;(out Width: Double; out Height: Double); safecall;
  Move,\                             ;(DeltaX: Double; DeltaY: Double); safecall;
  Duplicate,\                        ;(OffsetX: Double; OffsetY: Double): IVGShapeRange; safecall;
  Clone,\                            ;(OffsetX: Double; OffsetY: Double): IVGShapeRange; safecall;
  Group,\                            ;: IVGShape; safecall;
  RemoveAll,\                        ;; safecall;
  OrderToFront,\                     ;; safecall;
  OrderToBack,\                      ;; safecall;
  OrderForwardOne,\                  ;; safecall;
  OrderBackOne,\                     ;; safecall;
  OrderFrontOf,\                     ;(const Shape: IVGShape); safecall;
  OrderBackOf,\                      ;(const Shape: IVGShape); safecall;
  OrderReverse,\                     ;; safecall;
  Rotate,\                           ;(Angle: Double); safecall;
  RotateEx,\                         ;(Angle: Double; CenterX: Double; CenterY: Double); safecall;
  Skew,\                             ;(AngleX: Double; AngleY: Double); safecall;
  SkewEx,\                           ;(AngleX: Double; AngleY: Double; CenterX: Double; CenterY: Double); safecall;
  UngroupAll,\                       ;; safecall;
  Flip,\                             ;(Axes: cdrFlipAxes); safecall;
  Get_PositionX,\                    ;: Double; safecall;
  Set_PositionX,\                    ;(pVal: Double); safecall;
  Get_PositionY,\                    ;: Double; safecall;
  Set_PositionY,\                    ;(pVal: Double); safecall;
  Get_SizeWidth,\                    ;: Double; safecall;
  Set_SizeWidth,\                    ;(pVal: Double); safecall;
  Get_SizeHeight,\                   ;: Double; safecall;
  Set_SizeHeight,\                   ;(pVal: Double); safecall;
  Get_RotationCenterX,\              ;: Double; safecall;
  Set_RotationCenterX,\              ;(pVal: Double); safecall;
  Get_RotationCenterY,\              ;: Double; safecall;
  Set_RotationCenterY,\              ;(pVal: Double); safecall;
  AddToPowerClip,\                   ;(const Shape: IVGShape; CenterInContainer: cdrTriState); safecall;
  RemoveFromContainer,\              ;(Level: Integer); safecall;
  AddRange,\                         ;(const ShapeRange: IVGShapeRange); safecall;
  SetPosition,\                      ;(PositionX: Double; PositionY: Double); safecall;
  SetSize,\                          ;(Width: Double; Height: Double); safecall;
  ConvertToBitmapEx,\                ;(Mode: cdrImageType; Dithered: WordBool; Transparent: WordBool; Resolution: Integer; AntiAliasing: cdrAntiAliasingType; UseColorProfile: WordBool; AlwaysOverprintBlack: WordBool; OverprintBlackLimit: Integer): IVGShape; safecall;
  Combine,\                          ;: IVGShape; safecall;
  SetBoundingBox,\                   ;(x: Double; y: Double; Width: Double; Height: Double; KeepAspect: WordBool; ReferencePoint: cdrReferencePoint); safecall;
  ApplyNoFill,\                      ;; safecall;
  ApplyUniformFill,\                 ;(const Color: IVGColor); safecall;
  ApplyFountainFill,\                ;(const StartColor: IVGColor; const EndColor: IVGColor; Type_: cdrFountainFillType; Angle: Double; Steps: Integer; EdgePad: Integer; MidPoint: Integer; BlendType: cdrFountainFillBlendType; CenterOffsetX: Double; CenterOffsetY: Double); safecall;
  ApplyPatternFill,\                 ;(Type_: cdrPatternFillType; const FileName: WideString; PatternCanvasIndex: Integer; const FrontColor: IVGColor; const EndColor: IVGColor; TransformWithShape: WordBool); safecall;
  ApplyTextureFill,\                 ;(const TextureName: WideString; const LibraryName: WideString); safecall;
  ApplyPostscriptFill,\              ;(IndexOrName: OleVariant); safecall;
  ConvertOutlineToObject,\           ;: IVGShapeRange; safecall;
  SetOutlineProperties,\             ;(Width: Double; const Style: IVGOutlineStyle; const Color: IVGColor; const StartArrow: IVGArrowHead; const EndArrow: IVGArrowHead; BehindFill: cdrTriState; ScaleWithShape: cdrTriState; LineCaps: cdrOutlineLineCaps; LineJoin: cdrOutlineLineJoin; NibAngle: Double; NibStretch: Integer; DashDotLength: Double; PenWidth: Double; MiterLimit: Double); safecall;
  CreateSelection,\                  ;; safecall;
  RemoveFromSelection,\              ;; safecall;
  SetRotationCenter,\                ;(x: Double; y: Double); safecall;
  Stretch,\                          ;(StretchX: Double; StretchY: Double; StretchCharactersSize: WordBool); safecall;
  StretchEx,\                        ;(CenterX: Double; CenterY: Double; StretchX: Double; StretchY: Double; StretchCharactersSize: WordBool); safecall;
  SetSizeEx,\                        ;(CenterX: Double; CenterY: Double; Width: Double; Height: Double); safecall;
  GetBoundingBox,\                   ;(out x: Double; out y: Double; out Width: Double; out Height: Double; UseOutline: WordBool); safecall;
  RestoreCloneLink,\                 ;(LinkToRestore: cdrCloneLinkType); safecall;
  ClearEffect,\                      ;(Type_: cdrEffectType); safecall;
  RemoveRange,\                      ;(const Range: IVGShapeRange); safecall;
  DeleteItem,\                       ;(Index: Integer); safecall;
  CustomCommand,\                    ;(const ComponentID: WideString; const CommandID: WideString; var Parameters: PSafeArray): OleVariant; safecall;
  AlignToShape,\                     ;(Type_: cdrAlignType; const Shape: IVGShape; TextAlignOrigin: cdrTextAlignOrigin); safecall;
  AlignToShapeRange,\                ;(Type_: cdrAlignType; const ShapeRange: IVGShapeRange; TextAlignOrigin: cdrTextAlignOrigin); safecall;
  AlignToPage,\                      ;(Type_: cdrAlignType; TextAlignOrigin: cdrTextAlignOrigin); safecall;
  AlignToPageCenter,\                ;(Type_: cdrAlignType; TextAlignOrigin: cdrTextAlignOrigin); safecall;
  AlignToGrid,\                      ;(Type_: cdrAlignType; TextAlignOrigin: cdrTextAlignOrigin); safecall;
  AlignToPoint,\                     ;(Type_: cdrAlignType; x: Double; y: Double; TextAlignOrigin: cdrTextAlignOrigin); safecall;
  Distribute,\                       ;(Type_: cdrDistributeType; PageExtent: WordBool); safecall;
  ConvertToSymbol,\                  ;(const Name: WideString): IVGShape; safecall;
  Ungroup,\                          ;; safecall;
  UngroupEx,\                        ;: IVGShapeRange; safecall;
  UngroupAllEx,\                     ;: IVGShapeRange; safecall;
  Range,\                            ;(var IndexArray: PSafeArray): IVGShapeRange; safecall;
  All,\                              ;: IVGShapeRange; safecall;
  AllExcluding,\                     ;(var IndexArray: PSafeArray): IVGShapeRange; safecall;
  BreakApart,\                       ;; safecall;
  BreakApartEx,\                     ;: IVGShapeRange; safecall;
  MoveToLayer,\                      ;(const Layer: IVGLayer); safecall;
  CopyToLayer,\                      ;(const Layer: IVGLayer): IVGShapeRange; safecall;
  ClearTransformations,\             ;; safecall;
  Lock,\                             ;; safecall;
  Unlock,\                           ;; safecall;
  AlignRangeToShape,\                ;(Type_: cdrAlignType; const Shape: IVGShape); safecall;
  AlignRangeToShapeRange,\           ;(Type_: cdrAlignType; const ShapeRange: IVGShapeRange); safecall;
  AlignRangeToPage,\                 ;(Type_: cdrAlignType); safecall;
  AlignRangeToPageCenter,\           ;(Type_: cdrAlignType); safecall;
  AlignRangeToGrid,\                 ;(Type_: cdrAlignType); safecall;
  AlignRangeToPoint,\                ;(Type_: cdrAlignType; x: Double; y: Double); safecall;
  ApplyEffectInvert,\                ;; safecall;
  ApplyEffectPosterize,\             ;(Level: Integer); safecall;
  ApplyEffectBCI,\                   ;(Brighness: Integer; Contrast: Integer; Intensity: Integer); safecall;
  ApplyEffectColorBalance,\          ;(CyanRed: Integer; MagentaGreen: Integer; YellowBlue: Integer; ApplyToShadows: WordBool; ApplyToMidtones: WordBool; ApplyToHighlights: WordBool; PreserveLuminance: WordBool); safecall;
  ApplyEffectGamma,\                 ;(Gamma: Double); safecall;
  ApplyEffectHSL,\                   ;(Hue: OleVariant; Saturation: OleVariant; Lightness: OleVariant); safecall;
  AffineTransform,\                  ;(d11: Double; d12: Double; d21: Double; d22: Double; CenterX: Double; CenterY: Double); safecall;
  ApplyFill,\                        ;(const Fill: IVGFill); safecall;
  ApplyOutline,\                     ;(const Outline: IVGOutline); safecall;
  Get_ReverseRange,\                 ;: IVGShapeRange; safecall;
  Fillet,\                           ;(Radius: Double; CombineSmoothSegments: WordBool); safecall;
  Chamfer,\                          ;(DistanceA: Double; DistanceB: Double; CombineSmoothSegments: WordBool); safecall;
  Scallop,\                          ;(Radius: Double; CombineSmoothSegments: WordBool); safecall;
  SetFillMode,\                      ;(Mode: cdrFillMode); safecall;
  ApplyCustomHatchFill,\             ;(Angle: Double; Spacing: Double; Shift: Double; OriginX: Double; OriginY: Double; Width: Double; const Color: IVGColor; const Style: IVGOutlineStyle; DashDotLength: Double; PenWidth: Double; const BackColor: IVGColor; TransformWithShape: WordBool; ScaleLinesWithShape: WordBool; UseWorldCoordinates: WordBool; FillScale: Double; LineScale: Double; FillAngle: Double; FillSkew: Double); safecall;
  ApplyHatchFill,\                   ;(const LibraryName: WideString; HatchNameOrIndex: OleVariant; const BackColor: IVGColor; TransformWithShape: WordBool; ScaleLinesWithShape: WordBool; UseWorldCoordinates: WordBool; FillScale: Double; LineScale: Double; FillAngle: Double; FillSkew: Double); safecall;
  Get_LeftX,\                        ;: Double; safecall;
  Get_RightX,\                       ;: Double; safecall;
  Get_TopY,\                         ;: Double; safecall;
  Get_BottomY,\                      ;: Double; safecall;
  Get_Shapes,\                       ;: IVGShapes; safecall;
  Get_FirstShape,\                   ;: IVGShape; safecall;
  Get_LastShape,\                    ;: IVGShape; safecall;
  StepAndRepeat,\                    ;(NumCopies: Integer; DistanceX: Double; DistanceY: Double; ModeX: cdrDistanceMode; DirectionX: cdrDirection; ModeY: cdrDistanceMode; DirectionY: cdrDirection): IVGShapeRange; safecall;
  Exists,\                           ;(const Shape: IVGShape): WordBool; safecall;
  ExistsAnyOfType,\                  ;(var TypeList: PSafeArray): WordBool; safecall;
  CountAnyOfType,\                   ;(var TypeList: PSafeArray): Integer; safecall;
  FindAnyOfType,\                    ;(var TypeList: PSafeArray): IVGShapeRange; safecall;
  GetLinkedShapes,\                  ;(LinkType: cdrShapeLinkType): IVGShapeRange; safecall;
  Get_BoundingBox,\                  ;: IVGRect; safecall;
  GetPositionEx,\                    ;(ReferencePoint: cdrReferencePoint; out x: Double; out y: Double); safecall;
  SetPositionEx,\                    ;(ReferencePoint: cdrReferencePoint; x: Double; y: Double); safecall;
  Get_CenterX,\                      ;: Double; safecall;
  Set_CenterX,\                      ;(pVal: Double); safecall;
  Get_CenterY,\                      ;: Double; safecall;
  Set_CenterY,\                      ;(pVal: Double); safecall;
  Set_LeftX,\                        ;(pVal: Double); safecall;
  Set_RightX,\                       ;(pVal: Double); safecall;
  Set_TopY,\                         ;(pVal: Double); safecall;
  Set_BottomY,\                      ;(pVal: Double); safecall;
  CopyPropertiesFrom,\               ;(const Source: IVGShape; Properties: cdrCopyProperties): WordBool; safecall;
  GetOverprintFillState,\            ;: cdrOverprintState; safecall;
  GetOverprintOutlineState,\         ;: cdrOverprintState; safecall;
  Sort,\                             ;(const CompareExpression: WideString; StartIndex: Integer; EndIndex: Integer; Data: OleVariant); safecall;
  SetPixelAlignedRendering,\         ;(PixelAligned: WordBool); safecall;
  CreateDocumentFrom,\               ;(TemporaryDocument: WordBool): IVGDocument; safecall;
  AlignAndDistribute,\               ;(MethodH: cdrAlignDistributeH; MethodV: cdrAlignDistributeV; AlignTo: cdrAlignShapesTo; DistributeArea: cdrDistributeArea; UseOutline: WordBool; TextAlignOrigin: cdrTextAlignOrigin; PointX: Double; PointY: Double; const DistributeRect: IVGRect); safecall;
  SetOutlinePropertiesEx,\           ;(Width: Double; const Style: IVGOutlineStyle; const Color: IVGColor; const StartArrow: IVGArrowHead; const EndArrow: IVGArrowHead; BehindFill: cdrTriState; ScaleWithShape: cdrTriState; LineCaps: cdrOutlineLineCaps; LineJoin: cdrOutlineLineJoin; NibAngle: Double; NibStretch: Integer; DashDotLength: Double; PenWidth: Double; MiterLimit: Double; Justification: cdrOutlineJustification); safecall;
  CreateBoundary,\                   ;(x: Double; y: Double; PlaceOnTop: WordBool; DeleteSource: WordBool): IVGShape; safecall;
  EqualDivide,\                      ;(Divisions: Integer; Gap: Double; Group: WordBool; Combine: WordBool; DeleteSource: WordBool): IVGShapeRange; safecall;
  Project,\                          ;(Plane: cdrProjectPlane; ReferencePoint: cdrReferencePoint; ApplyToDuplicate: WordBool): IVGShapeRange; safecall;
  Unproject,\                        ;(Plane: cdrProjectPlane; ReferencePoint: cdrReferencePoint; ApplyToDuplicate: WordBool): IVGShapeRange; safecall;
  Show,\                             ;; safecall;
  Hide,\                             ;; safecall;
  GetToolShapes,\                    ;(const ShapeGuid: WideString): IVGShapeRange; safecall;
  ModifyToolShapeProperties,\        ;(const ShapePropertiesToModify: IVGProperties); safecall;
  CreateParallelCurves               ;(Count: Integer; distanceBetweenCurves: Double): IVGShapeRange; safecall;

interface IVGWindow,\
  QueryInterface,\
  AddRef,\
  Release,\
  GetTypeInfoCount,\
  GetTypeInfo,\
  GetIDsOfNames,\
  Invoke,\
  Get_Application,\                   ;: IDispatch; safecall;
  Get_Parent,\                        ;: IVGWindows; safecall;
  Activate,\                          ; safecall;
  Close,\                             ; safecall;
  Get_FullScreen,\                    ;: WordBool; safecall;
  Set_FullScreen,\                    ;(FullScreen: WordBool); safecall;
  Get_Page,\                          ;: IDispatch; safecall;
  Get_Active,\                        ;: WordBool; safecall;
  Get_Caption,\                       ;: WideString; safecall;
  Get_Height,\                        ;: Integer; safecall;
  Set_Height,\                        ;(pVal: Integer); safecall;
  Get_Width,\                         ;: Integer; safecall;
  Set_Width,\                         ;(pVal: Integer); safecall;
  Get_Left,\                          ;: Integer; safecall;
  Set_Left,\                          ;(pVal: Integer); safecall;
  Get_Top,\                           ;: Integer; safecall;
  Set_Top,\                           ;(pVal: Integer); safecall;
  Get_WindowState,\                   ;: cdrWindowState; safecall;
  Set_WindowState,\                   ;(pVal: cdrWindowState); safecall;
  Get_Previous,\                      ;: IVGWindow; safecall;
  Get_Next,\                          ;: IVGWindow; safecall;
  Get_Index,\                         ;: Integer; safecall;
  NewWindow,\                         ;: IVGWindow; safecall;
  Refresh,\                           ; safecall;
  Get_Document,\                      ;: IVGDocument; safecall;
  Get_ActiveView,\                    ;: IVGActiveView; safecall;
  ScreenToDocument,\                  ;(XScreen: Integer; YScreen: Integer; out XDoc: Double; out YDoc: Double); safecall;
  DocumentToScreen,\                  ;(XDoc: Double; YDoc: Double; out XScreen: Integer; out YScreen: Integer); safecall;
  Get_Handle,\                        ;: Integer; safecall;
  Get_ViewWindow,\                    ;: ICUIViewWindow; safecall;
  ScreenDistanceToDocumentDistance,\  ;(ScreenDistance: Double): Double; safecall;
  DocumentDistanceToScreenDistance    ;(DocumentDistance: Double): Double; safecall;

interface IVGDocument,\
  QueryInterface,\
  AddRef,\
  Release,\
  GetTypeInfoCount,\
  GetTypeInfo,\
  GetIDsOfNames,\
  Invoke,\
  Get_Application,\                        ;: IVGApplication; safecall;
  Get_Parent,\                             ;: IVGDocuments; safecall;
  Get_Name,\                               ;: WideString; safecall;
  SaveAs,\                                 ;(const FileName: WideString; const Options: IVGStructSaveAsOptions); safecall;
  Save,\                                   ;; safecall;
  Get_Pages,\                              ;: IVGPages; safecall;
  Get_ReferencePoint,\                     ;: cdrReferencePoint; safecall;
  Set_ReferencePoint,\                     ;(pRefPoint: cdrReferencePoint); safecall;
  Get_ApplyToDuplicate,\                   ;: WordBool; safecall;
  Set_ApplyToDuplicate,\                   ;(ApplyToDuplicate: WordBool); safecall;
  Get_ActivePage,\                         ;: IVGPage; safecall;
  Get_ActiveLayer,\                        ;: IVGLayer; safecall;
  Get_Windows,\                            ;: IVGWindows; safecall;
  Get_ActiveWindow,\                       ;: IVGWindow; safecall;
  Close,\                                  ;; safecall;
  Undo,\                                   ;(Levels: Integer); safecall;
  Redo,\                                   ;(Levels: Integer); safecall;
  Repeat_,\                                ;; safecall;
  Activate,\                               ;; safecall;
  Get_Unit,\                              ;: cdrUnit; safecall;
  Set_Unit,\                              ;(pnUnit: cdrUnit); safecall;
  Get_DrawingOriginX,\                     ;: Double; safecall;
  Set_DrawingOriginX,\                     ;(plX: Double); safecall;
  Get_DrawingOriginY,\                     ;: Double; safecall;
  Set_DrawingOriginY,\                     ;(plY: Double); safecall;
  AddPages,\                               ;(NumberOfPages: Integer): IVGPage; safecall;
  InsertPages,\                            ;(NumberOfPages: Integer; BeforePage: WordBool; Page: Integer): IVGPage; safecall;
  Selection,\                              ;: IVGShape; safecall;
  ClearSelection,\                         ;; safecall;
  Export,\                                 ;(const FileName: WideString; Filter: cdrFilter; Range: cdrExportRange; const Options: IVGStructExportOptions; const PaletteOptions: IVGStructPaletteOptions); safecall;
  ResolveAllBitmapsLinks,\                 ;; safecall;
  Get_Dirty,\                              ;: WordBool; safecall;
  Set_Dirty,\                              ;(Dirty: WordBool); safecall;
  GetUserClick,\                           ;(out x: Double; out y: Double; out ShiftState: Integer; TimeOut: Integer; Snap: WordBool; CursorShape: cdrCursorShape): Integer; safecall;
  GetUserArea,\                            ;(out x1: Double; out y1: Double; out x2: Double; out y2: Double; out ShiftState: Integer; TimeOut: Integer; Snap: WordBool; CursorShape: cdrCursorShape): Integer; safecall;
  BeginCommandGroup,\                      ;(const CommandName: WideString); safecall;
  EndCommandGroup,\                        ;; safecall;
  Get_FilePath,\                           ;: WideString; safecall;
  Get_FileName,\                           ;: WideString; safecall;
  Get_FullFileName,\                       ;: WideString; safecall;
  Get_Resolution,\                         ;: Integer; safecall;
  Set_Resolution,\                         ;(pResolution: Integer); safecall;
  Get_ShapeEnumDirection,\                 ;: cdrShapeEnumDirection; safecall;
  Set_ShapeEnumDirection,\                 ;(peDirection: cdrShapeEnumDirection); safecall;
  Get_SelectionRange,\                     ;: IVGShapeRange; safecall;
  Get_Rulers,\                             ;: IVGRulers; safecall;
  Get_Grid,\                               ;: IVGGrid; safecall;
  Get_Views,\                              ;: IVGViews; safecall;
  CreateView,\                             ;(const Name: WideString; OriginX: Double; OriginY: Double; Zoom: Integer; const Page: IVGPage): IVGView; safecall;
  Get_ActivePowerClip,\                    ;: IVGPowerClip; safecall;
  AdviseEvents,\                           ;(const EventSink: IDispatch): Integer; safecall;
  UnadviseEvents,\                         ;(Cookie: Integer); safecall;
  Get_WorldScale,\                         ;: Double; safecall;
  Set_WorldScale,\                         ;(Scale: Double); safecall;
  PrintOut,\                               ;; safecall;
  Get_ActiveShape,\                        ;: IVGShape; safecall;
  Get_CurvePrecision,\                     ;: Integer; safecall;
  AddPagesEx,\                             ;(NumberOfPages: Integer; Width: Double; Height: Double): IVGPage; safecall;
  InsertPagesEx,\                          ;(NumberOfPages: Integer; BeforePage: WordBool; Page: Integer; Width: Double; Height: Double): IVGPage; safecall;
  Get_Title,\                              ;: WideString; safecall;
  SaveSettings,\                           ;(const Tag: WideString); safecall;
  RestoreSettings,\                        ;(const Tag: WideString); safecall;
  Get_Active,\                             ;: WordBool; safecall;
  Get_Index,\                              ;: Integer; safecall;
  ExportEx,\                               ;(const FileName: WideString; Filter: cdrFilter; Range: cdrExportRange; const Options: IVGStructExportOptions; const PaletteOptions: IVGStructPaletteOptions): ICorelExportFilter; safecall;
  ExportBitmap,\                           ;(const FileName: WideString; Filter: cdrFilter; Range: cdrExportRange; ImageType: cdrImageType; Width: Integer; Height: Integer; ResolutionX: Integer; ResolutionY: Integer; AntiAliasingType: cdrAntiAliasingType; Dithered: WordBool; Transparent: WordBool; UseColorProfile: WordBool; MaintainLayers: WordBool; Compression: cdrCompressionType; const PaletteOptions: IVGStructPaletteOptions; const ExportArea: IVGRect): ICorelExportFilter; safecall;
  Get_EditAcrossLayers,\                   ;: WordBool; safecall;
  Set_EditAcrossLayers,\                   ;(pVal: WordBool); safecall;
  Get_Properties,\                         ;: IVGProperties; safecall;
  Set_CurvePrecision,\                     ;(lpPrec: Integer); safecall;
  Get_PrintSettings,\                      ;: IPrnVBAPrintSettings; safecall;
  Get_Keywords,\                           ;: WideString; safecall;
  Set_Keywords,\                           ;(const pVal: WideString); safecall;
  Get_Notes,\                              ;: WideString; safecall;
  Set_Notes,\                              ;(const pVal: WideString); safecall;
  Get_PreserveSelection,\                  ;: WordBool; safecall;
  Set_PreserveSelection,\                  ;(pVal: WordBool); safecall;
  ResetSettings,\                          ;; safecall;
  Get_DataFields,\                         ;: IVGDataFields; safecall;
  Get_PDFSettings,\                        ;: IPDFVBASettings; safecall;
  PublishToPDF,\                           ;(const FileName: WideString); safecall;
  Get_SelectionInfo,\                      ;: IVGSelectionInformation; safecall;
  Get_PageSizes,\                          ;: IVGPageSizes; safecall;
  Get_Components,\                         ;: IVGComponents; safecall;
  Get_SymbolLibrary,\                      ;: IVGSymbolLibrary; safecall;
  CreateCurve,\                            ;: IVGCurve; safecall;
  CreateCurveFromArray,\                   ;(var Source: PSafeArray; NumElements: Integer): IVGCurve; safecall;
  LoadStyleSheet,\                         ;(const FileName: WideString); safecall;
  SaveStyleSheet,\                         ;(const FileName: WideString); safecall;
  SaveStyleSheetAsDefault,\                ;; safecall;
  CreateSelection,\                        ;(var ShapeArray: PSafeArray); safecall;
  AddToSelection,\                         ;(var ShapeArray: PSafeArray); safecall;
  RemoveFromSelection,\                    ;(var ShapeArray: PSafeArray); safecall;
  Get_SelectableShapes,\                   ;: IVGShapes; safecall;
  ToUnits,\                                ;(Value: Double; FromUnit: cdrUnit): Double; safecall;
  FromUnits,\                              ;(Value: Double; ToUnit: cdrUnit): Double; safecall;
  Get_ResolutionX,\                        ;: Integer; safecall;
  Set_ResolutionX,\                        ;(pResolution: Integer); safecall;
  Get_ResolutionY,\                        ;: Integer; safecall;
  Set_ResolutionY,\                        ;(pResolution: Integer); safecall;
  Get_MasterPage,\                         ;: IVGPage; safecall;
  Revert,\                                 ;: WordBool; safecall;
  Get_CodeName,\                           ;: WideString; safecall;
  Get__CodeName,\                          ;: WideString; safecall;
  Set__CodeName,\                          ;(const pVal: WideString); safecall;
  Set_Name,\                               ;(const pbstrName: WideString); safecall;
  Set_FullFileName,\                       ;(const pFullFileName: WideString); safecall;
  Get_TreeRoot,\                           ;: IVGTreeNode; safecall;
  Get_TreeManager,\                        ;: IVGTreeManager; safecall;
  LogCreateShape,\                         ;(const VirtualShape: IVGShape): IVGShape; safecall;
  LogCreateShapeRange,\                    ;(const VirtualShapeRange: IVGShapeRange): IVGShapeRange; safecall;
  CreateFill,\                             ;(const FillString: WideString): IVGFill; safecall;
  CreateOutline,\                          ;(const OutlineString: WideString): IVGOutline; safecall;
  Get_HatchLibraries,\                     ;: IVGHatchLibraries; safecall;
  CreateShapeRangeFromArray,\              ;(var ShapeArray: PSafeArray): IVGShapeRange; safecall;
  ClearUndoList,\                          ;; safecall;
  Get_SourcePlatformVersion,\              ;: Double; safecall;
  Set_SourcePlatformVersion,\              ;(pVal: Double); safecall;
  Get_SourceFormat,\                       ;: cdrFilter; safecall;
  Get_IsCurrentVersion,\                   ;: WordBool; safecall;
  Get_SourceFileVersion,\                  ;: cdrFileVersion; safecall;
  Set_SourceFileVersion,\                  ;(pVal: cdrFileVersion); safecall;
  CreateUniformFill,\                      ;(const Color: IVGColor): IVGFill; safecall;
  Get_Metadata,\                           ;: IVGMetadata; safecall;
  Get_Layout,\                             ;: cdrDocLayout; safecall;
  Set_Layout,\                             ;(pVal: cdrDocLayout); safecall;
  Get_FacingPages,\                        ;: WordBool; safecall;
  Set_FacingPages,\                        ;(pVal: WordBool); safecall;
  Get_FirstPageOnRightSide,\               ;: WordBool; safecall;
  Set_FirstPageOnRightSide,\               ;(pVal: WordBool); safecall;
  SetLayout,\                              ;(Layout: cdrDocLayout; FacingPages: WordBool; StartOnRightSide: WordBool); safecall;
  Get_Spreads,\                            ;: IVGSpreads; safecall;
  Get_ActiveSpread,\                       ;: IVGSpread; safecall;
  CreateArrowHead,\                        ;(const Curve: IVGCurve): IVGArrowHead; safecall;
  CreateArrowHeadEx,\                      ;(const Curve: IVGCurve; CenterX: Double; CenterY: Double; OutlineWidthScale: Double; LineOffset: Double): IVGArrowHead; safecall;
  DeletePages,\                            ;(StartPage: Integer; NumPages: Integer): WordBool; safecall;
  CreateArrowHead2,\                       ;(const Curve: IVGCurve; const Name: WideString): IVGArrowHead; safecall;
  CreateArrowHeadEx2,\                     ;(const Curve: IVGCurve; const Name: WideString; CenterX: Double; CenterY: Double; OutlineWidthScale: Double; LineOffset: Double): IVGArrowHead; safecall;
  CreateArrowHeadOptions,\                 ;(Length: Double; Width: Double; OffsetX: Double; OffsetY: Double; RotationAngle: Double; FlipHorizontal: WordBool; FlipVertical: WordBool): IVGArrowHeadOptions; safecall;
  SaveAsCopy,\                             ;(const FileName: WideString; const Options: IVGStructSaveAsOptions); safecall;
  CreateFreeSnapPoint,\                    ;(PositionX: Double; PositionY: Double): IVGSnapPoint; safecall;
  CreateBSpline,\                          ;(NumControlPoints: Integer; Closed: WordBool): IVGBSpline; safecall;
  Duplicate,\                              ;: IVGDocument; safecall;
  Clone,\                                  ;: IVGDocument; safecall;
  Get_IsTemporary,\                        ;: WordBool; safecall;
  Get_ColorContext,\                       ;: IVGColorContext; safecall;
  AssignColorContext,\                     ;(const ColorContext: IVGColorContext); safecall;
  ConvertToColorContext,\                  ;(const ColorContext: IVGColorContext); safecall;
  PrintColorProof,\                        ;(const ProofSettings: IVGProofColorSettings); safecall;
  Get_Palette,\                            ;: IVGPalette; safecall;
  Get_TextFormatter,\                      ;: Integer; safecall;
  Set_TextFormatter,\                      ;(pVal: Integer); safecall;
  Get_StyleSheet,\                         ;: IVGStyleSheet; safecall;
  InteractiveImport,\                      ;(var FileNames: PSafeArray); safecall;
  AddColorsToDocPalette,\                  ;(SelectedOnly: WordBool; MaxColorsPerBitmap: Integer): WordBool; safecall;
  CreateColorStyles,\                      ;(UseFills: WordBool; UseOutlines: WordBool; SelectedOnly: WordBool; NumberOfColorHarmonies: Integer; ConvertColorsTo: cdrColorType): Integer; safecall;
  CreateCurveFitToPoints,\                 ;(const Points: IVGPointRange; UseCurrentViewForTolerance: WordBool; tolerance: Double): IVGCurve; safecall;
  CreateCurveFitToPointsAndCusps,\         ;(const Points: IVGPointRange; var CuspIndexArray: PSafeArray; UseCurrentViewForTolerance: WordBool; tolerance: Double): IVGCurve; safecall;
  SampleColorAtPoint,\                     ;(x: Double; y: Double; ColorType: cdrColorType): IVGColor; safecall;
  SampleColorInArea,\                      ;(x1: Double; y1: Double; x2: Double; y2: Double; XSamples: Integer; YSamples: Integer; ColorType: cdrColorType): IVGColor; safecall;
  ShowAllHiddenObjects,\                   ;; safecall;
  InteractiveImportWithContentIdentifier,\ ;(var FileNames: PSafeArray; var ContentIdentifiers: PSafeArray); safecall;
  ReplaceContentByIdentifier,\             ;(var ContentIdentifiers: PSafeArray; var FileNames: PSafeArray); safecall;
  Get_ContentIdentifiers,\                 ;: PSafeArray; safecall;
  CustomCommand,\                          ;(const ComponentID: WideString; const CommandID: WideString; var Parameters: PSafeArray): OleVariant; safecall;
  Get_Math,\                               ;: IVGMathUtils; safecall;
  CreateImage                              ;(ImageType: cdrImageType; Width: Integer; Height: Integer; const FillColor: IVGColor): IVGImage; safecall;

interface IVGLayer,\
  QueryInterface,\
  AddRef,\
  Release,\
  GetTypeInfoCount,\
  GetTypeInfo,\
  GetIDsOfNames,\
  Invoke,\
  Get_Application,\                        ;: IVGApplication; safecall;
  Get_Parent,\                             ;: IVGLayers; safecall;
  Get_Name,\                               ;: WideString; safecall;
  Set_Name,\                               ;(const pVal: WideString); safecall;
  Get_Shapes,\                             ;: IVGShapes; safecall;
  Activate,\                               ;; safecall;
  Get_Visible,\                            ;: WordBool; safecall;
  Set_Visible,\                            ;(pVal: WordBool); safecall;
  Get_Printable,\                          ;: WordBool; safecall;
  Set_Printable,\                          ;(pVal: WordBool); safecall;
  Get_Editable,\                           ;: WordBool; safecall;
  Set_Editable,\                           ;(pVal: WordBool); safecall;
  Get_Master,\                             ;: WordBool; safecall;
  Set_Master,\                             ;(pVal: WordBool); safecall;
  Get_OverrideColor,\                      ;: WordBool; safecall;
  Set_OverrideColor,\                      ;(pVal: WordBool); safecall;
  Get_Color,\                              ;: IVGColor; safecall;
  Set_Color,\                              ;(const pVal: IVGColor); safecall;
  Delete,\                                 ;; safecall;
  MoveAbove,\                              ;(const Layer: IVGLayer); safecall;
  MoveBelow,\                              ;(const Layer: IVGLayer); safecall;
  Import,\                                 ;(const FileName: WideString; Filter: cdrFilter; const Options: IVGStructImportOptions); safecall;
  CreateRectangle,\                        ;(Left: Double; Top: Double; Right: Double; Bottom: Double; CornerUL: Integer; CornerUR: Integer; CornerLR: Integer; CornerLL: Integer): IVGShape; safecall;
  CreateEllipse,\                          ;(Left: Double; Top: Double; Right: Double; Bottom: Double; StartAngle: Double; EndAngle: Double; Pie: WordBool): IVGShape; safecall;
  CreatePolygon,\                          ;(Left: Double; Top: Double; Right: Double; Bottom: Double; Sides: Integer; SubPaths: Integer; Complexity: Integer; Star: WordBool; StarComplexity: Integer; MaxComplexity: Integer): IVGShape; safecall;
  CreateGridBoxes,\                        ;(Left: Double; Top: Double; Right: Double; Bottom: Double; Wide: Integer; High: Integer): IVGShape; safecall;
  CreateSpiral,\                           ;(Left: Double; Top: Double; Right: Double; Bottom: Double; NumRevolutions: Integer; SpiralType: cdrSpiralType; GrowthRate: Integer): IVGShape; safecall;
  CreateArtisticText,\                     ;(Left: Double; Bottom: Double; const Text: WideString; LanguageID: cdrTextLanguage; CharSet: cdrTextCharSet; const Font: WideString; Size: Single; Bold: cdrTriState; Italic: cdrTriState; Underline: cdrFontLine; Alignment: cdrAlignment): IVGShape; safecall;
  CreateParagraphText,\                    ;(Left: Double; Top: Double; Right: Double; Bottom: Double; const Text: WideString; LanguageID: cdrTextLanguage; CharSet: cdrTextCharSet; const Font: WideString; Size: Single; Bold: cdrTriState; Italic: cdrTriState; Underline: cdrFontLine; Alignment: cdrAlignment): IVGShape; safecall;
  CreateCurveSegment,\                     ;(StartX: Double; StartY: Double; EndX: Double; EndY: Double; StartingControlPointLength: Double; StartingControlPointAngle: Double; EndingControlPointLength: Double; EndingControlPointAngle: Double): IVGShape; safecall;
  CreateLineSegment,\                      ;(StartX: Double; StartY: Double; EndX: Double; EndY: Double): IVGShape; safecall;
  CreateConnector,\                        ;(const Start: IVGSnapPoint; const End_: IVGSnapPoint): IVGShape; safecall;
  CreateCurve,\                            ;(const Source: IVGCurve): IVGShape; safecall;
  Paste,\                                  ;: IVGShape; safecall;
  CreateGuideAngle,\                       ;(x: Double; y: Double; Angle: Double): IVGShape; safecall;
  CreateGuide,\                            ;(x1: Double; y1: Double; x2: Double; y2: Double): IVGShape; safecall;
  CreateEllipse2,\                         ;(CenterX: Double; CenterY: Double; Radius1: Double; Radius2: Double; StartAngle: Double; EndAngle: Double; Pie: WordBool): IVGShape; safecall;
  FindShape,\                              ;(const Name: WideString; Type_: cdrShapeType; StaticID: Integer; Recursive: WordBool): IVGShape; safecall;
  FindShapes,\                             ;(const Name: WideString; Type_: cdrShapeType; Recursive: WordBool): IVGShapeRange; safecall;
  CreateRectangle2,\                       ;(x: Double; y: Double; Width: Double; Height: Double; RadiusUL: Double; RadiusUR: Double; RadiusLR: Double; RadiusLL: Double): IVGShape; safecall;
  CreateFreeConnector,\                    ;(x1: Double; y1: Double; x2: Double; y2: Double): IVGShape; safecall;
  Get_Properties,\                         ;: IVGProperties; safecall;
  Get_MasterProperties,\                   ;: IVGProperties; safecall;
  Get_Index,\                              ;: Integer; safecall;
  CreateCurveSegment2,\                    ;(x1: Double; y1: Double; StartingControlPointX: Double; StartingControlPointY: Double; EndingControlPointX: Double; EndingControlPointY: Double; x2: Double; y2: Double): IVGShape; safecall;
  ImportEx,\                               ;(const FileName: WideString; Filter: cdrFilter; const Options: IVGStructImportOptions): ICorelImportFilter; safecall;
  CreateArtisticTextWide,\                 ;(Left: Double; Bottom: Double; const Text: WideString; LanguageID: cdrTextLanguage; CharSet: cdrTextCharSet; const Font: WideString; Size: Single; Bold: cdrTriState; Italic: cdrTriState; Underline: cdrFontLine; Alignment: cdrAlignment): IVGShape; safecall;
  CreateParagraphTextWide,\                ;(Left: Double; Top: Double; Right: Double; Bottom: Double; const Text: WideString; LanguageID: cdrTextLanguage; CharSet: cdrTextCharSet; const Font: WideString; Size: Single; Bold: cdrTriState; Italic: cdrTriState; Underline: cdrFontLine; Alignment: cdrAlignment): IVGShape; safecall;
  CustomCommand,\                          ;(const ComponentID: WideString; const CommandID: WideString; var Parameters: PSafeArray): OleVariant; safecall;
  CreateCustomShape,\                      ;(const TypeID: WideString; var Parameters: PSafeArray): IVGShape; safecall;
  CreateLinearDimension,\                  ;(Type_: cdrLinearDimensionType; const Point1: IVGSnapPoint; const Point2: IVGSnapPoint; TextCentered: WordBool; TextX: Double; TextY: Double; Style: cdrDimensionStyle; Precision: Integer; ShowUnits: WordBool; Units: cdrDimensionLinearUnits; Placement: cdrDimensionPlacement; HorizontalText: WordBool; BoxedText: WordBool; LeadingZero: WordBool; const Prefix: WideString; const Suffix: WideString; OutlineWidth: Double; const Arrows: IVGArrowHead; const OutlineColor: IVGColor; const TextFont: WideString; TextSize: Double; const TextColor: IVGColor): IVGShape; safecall;
  CreateAngularDimension,\                 ;(const Center: IVGSnapPoint; const Point1: IVGSnapPoint; const Point2: IVGSnapPoint; TextX: Double; TextY: Double; Precision: Integer; ShowUnits: WordBool; Units: cdrDimensionAngularUnits; BoxedText: WordBool; LeadingZero: WordBool; const Prefix: WideString; const Suffix: WideString; OutlineWidth: Double; const Arrows: IVGArrowHead; const OutlineColor: IVGColor; const TextFont: WideString; TextSize: Double; const TextColor: IVGColor): IVGShape; safecall;
  CreateSymbol,\                           ;(x: Double; y: Double; const SymbolName: WideString; const Library_: IVGSymbolLibrary): IVGShape; safecall;
  CreatePolygon2,\                         ;(CenterX: Double; CenterY: Double; Radius: Double; Sides: Integer; Angle: Double; InnerRadius: Double; Star: WordBool; Sharpness: Integer): IVGShape; safecall;
  PasteSpecial,\                           ;(const FormatName: WideString; PasteLink: WordBool; DisplayAsIcon: WordBool; const Caption: WideString; Icon: OleVariant): WordBool; safecall;
  CreateOLEObject,\                        ;(const ObjectID: WideString; DisplayAsIcon: WordBool; const Caption: WideString; Icon: OleVariant): IVGShape; safecall;
  CreateOLEObjectFromFile,\                ;(const FileName: WideString; Link: WordBool; DisplayAsIcon: WordBool; const Caption: WideString; Icon: OleVariant): IVGShape; safecall;
  Get_SelectableShapes,\                   ;: IVGShapes; safecall;
  Get_TreeNode,\                           ;: IVGTreeNode; safecall;
  Get_IsGuidesLayer,\                      ;: WordBool; safecall;
  Get_IsDesktopLayer,\                     ;: WordBool; safecall;
  Get_IsGridLayer,\                        ;: WordBool; safecall;
  Get_IsSpecialLayer,\                     ;: WordBool; safecall;
  Get_MasterLayer,\                        ;: IVGLayer; safecall;
  Get_AbsoluteIndex,\                      ;: Integer; safecall;
  Get_Page,\                               ;: IVGPage; safecall;
  Get_Above,\                              ;(IgnoreMasters: WordBool): IVGLayer; safecall;
  Get_Below,\                              ;(IgnoreMasters: WordBool): IVGLayer; safecall;
  CreateRectangleRect,\                    ;(const Rect: IVGRect; RadiusUL: Double; RadiusUR: Double; RadiusLR: Double; RadiusLL: Double): IVGShape; safecall;
  CreateEllipseRect,\                      ;(const Rect: IVGRect; StartAngle: Double; EndAngle: Double; Pie: WordBool): IVGShape; safecall;
  CreateConnectorEx,\                      ;(Type_: cdrConnectorType; const Start: IVGSnapPoint; const End_: IVGSnapPoint): IVGShape; safecall;
  CreateRightAngleConnector,\              ;(const Start: IVGSnapPoint; const End_: IVGSnapPoint; CornerRadius: Double): IVGShape; safecall;
  CreateBSpline,\                          ;(const Source: IVGBSpline): IVGShape; safecall;
  PasteEx,\                                ;(const Options: IVGStructPasteOptions): IVGShapeRange; safecall;
  CreateToolShape,\                        ;(const ToolShapeGuid: WideString; const ShapeProperties: IVGProperties): IVGShape; safecall;
  CreateBitmap,\                           ;(Left: Double; Top: Double; Right: Double; Bottom: Double; const Image: IVGImage; const ImageAlpha: IVGImage): IVGShape; safecall;
  CreateBitmap2,\                          ;(x: Double; y: Double; Width: Double; Height: Double; const Image: IVGImage; const ImageAlpha: IVGImage): IVGShape; safecall;
  CreateBitmapRect                         ;(const Rect: IVGRect; const Image: IVGImage; const ImageAlpha: IVGImage): IVGShape; safecall;

interface IVGStyleSheet,\
  QueryInterface,\
  AddRef,\
  Release,\
  GetTypeInfoCount,\
  GetTypeInfo,\
  GetIDsOfNames,\
  Invoke,\
  Get_Styles,\                             ;: IVGStyles; safecall;
  Get_StyleSets,\                          ;: IVGStyles; safecall;
  Get_ObjectDefaults,\                     ;: IVGStyles; safecall;
  Get_AllStyles,\                          ;: IVGStyles; safecall;
  Get_AllStyleSets,\                       ;: IVGStyles; safecall;
  FindStyle,\                              ;(const Name: WideString): IVGStyle; safecall;
  CreateStyleFromShape,\                   ;(const Shape: IVGShape; const Category: WideString; const Name: WideString; ReplaceExisting: WordBool): IVGStyles; safecall;
  CreateStyleFromShapeRange,\              ;(const ShapeRange: IVGShapeRange; const Category: WideString; const Name: WideString; ReplaceExisting: WordBool): IVGStyles; safecall;
  CreateStyleFromTextRange,\               ;(const TextRange: IVGTextRange; const Category: WideString; const Name: WideString; ReplaceExisting: WordBool): IVGStyles; safecall;
  CreateStyleSetFromShape,\                ;(const Shape: IVGShape; const Name: WideString; ReplaceExisting: WordBool): IVGStyles; safecall;
  CreateStyleSetFromShapeRange,\           ;(const ShapeRange: IVGShapeRange; const Name: WideString; ReplaceExisting: WordBool): IVGStyles; safecall;
  CreateStyleSetFromTextRange,\            ;(const TextRange: IVGTextRange; const Name: WideString; ReplaceExisting: WordBool): IVGStyles; safecall;
  CreateStyle,\                            ;(const Category: WideString; const BasedOn: WideString; const Name: WideString; ReplaceExisting: WordBool): IVGStyle; safecall;
  CreateStyleSet,\                         ;(const BasedOn: WideString; const Name: WideString; ReplaceExisting: WordBool): IVGStyle; safecall;
  Export,\                                 ;(const FileName: WideString; Styles: WordBool; StyleSets: WordBool; ObjectDefaults: WordBool; ColorStyles: WordBool): WordBool; safecall;
  Import,\                                 ;(const FileName: WideString; MergeStyles: WordBool; Styles: WordBool; StyleSets: WordBool; ObjectDefaults: WordBool; ColorStyles: WordBool): WordBool; safecall;
  Get_AllColorStyles,\                     ;: IVGColors; safecall;
  CreateColorStyle,\                       ;(const Name: WideString; const Color: IVGColor; HarmonyIndex: Integer; IndexInHarmony: Integer; ReplaceExisting: WordBool): IVGColor; safecall;
  DeleteAllColorStyles,\                   ;; safecall;
  DeleteColorStyle,\                       ;(const Name: WideString); safecall;
  RenameColorStyle                         ;(const OldName: WideString; const NewName: WideString); safecall;

interface IVGStyles,\
  QueryInterface,\
  AddRef,\
  Release,\
  GetTypeInfoCount,\
  GetTypeInfo,\
  GetIDsOfNames,\
  Invoke,\
  Get_Count,\                              ;: Integer; safecall;
  Get_Item,\                               ;(Index: Integer): IVGStyle; safecall;
  Get__NewEnum,\                           ;: IUnknown; safecall;
  Find,\                                   ;(const Name: WideString): IVGStyle; safecall;
  Get_First,\                              ;: IVGStyle; safecall;
  Get_Last                                 ;: IVGStyle; safecall;

interface IVGStyle,\
  QueryInterface,\
  AddRef,\
  Release,\
  GetTypeInfoCount,\
  GetTypeInfo,\
  GetIDsOfNames,\
  Invoke,\
  Get_CategoryName,\                       ;: WideString; safecall;
  GetAllPropertyNames,\                    ;: PSafeArray; safecall;
  GetOverridePropertyNames,\               ;: PSafeArray; safecall;
  IsPropertyInherited,\                    ;(const Name: WideString): WordBool; safecall;
  GetProperty,\                            ;(const Name: WideString): OleVariant; safecall;
  SetProperty,\                            ;(const Name: WideString; Value: OleVariant); safecall;
  ClearProperty,\                          ;(const Name: WideString): WordBool; safecall;
  Get_Outline,\                            ;: IVGStyleOutline; safecall;
  Get_Fill,\                               ;: IVGStyleFill; safecall;
  Get_Character,\                          ;: IVGStyleCharacter; safecall;
  Get_Paragraph,\                          ;: IVGStyleParagraph; safecall;
  Get_Frame,\                              ;: IVGStyleFrame; safecall;
  Get_Name,\                               ;: WideString; safecall;
  Get_DisplayName,\                        ;: WideString; safecall;
  Get_IsStyleSet,\                         ;: WordBool; safecall;
  Get_IsObjectDefaults,\                   ;: WordBool; safecall;
  Get_DisplayCategoryName,\                ;: WideString; safecall;
  Get_BasedOn,\                            ;: IVGStyle; safecall;
  Get_DerivedStyles,\                      ;: IVGStyles; safecall;
  ToString,\                               ;: WideString; safecall;
  StringAssign,\                           ;(const StyleString: WideString): WordBool; safecall;
  GetPropertyAsString,\                    ;(const Name: WideString): WideString; safecall;
  SetPropertyAsString,\                    ;(const Name: WideString; const Value: WideString): WordBool; safecall;
  Rename,\                                 ;(const NewName: WideString): WordBool; safecall;
  SetBasedOn,\                             ;(const NewParent: WideString): WordBool; safecall;
  Delete,\                                 ;: WordBool; safecall;
  Assign,\                                 ;(const pVal: IVGStyle); safecall;
  GetCopy,\                                ;: IVGStyle; safecall;
  Get_Transparency                         ;: IVGStyleTransparency; safecall;

interface IVGOutline,\
  QueryInterface,\
  AddRef,\
  Release,\
  GetTypeInfoCount,\
  GetTypeInfo,\
  GetIDsOfNames,\
  Invoke,\
  Get_Width,\                              ;: Double; safecall;
  Set_Width,\                              ;(pVal: Double); safecall;
  Get_Color,\                              ;: IVGColor; safecall;
  Set_Color,\                              ;(const ppVal: IVGColor); safecall;
  ConvertToObject,\                        ;: IVGShape; safecall;
  Get_type,\                               ;: cdrOutlineType; safecall;
  Set_type,\                               ;(Type_: cdrOutlineType); safecall;
  Get_Style,\                              ;: IVGOutlineStyle; safecall;
  Set_Style,\                              ;(const ppStyle: IVGOutlineStyle); safecall;
  Get_StartArrow,\                         ;: IVGArrowHead; safecall;
  Set_StartArrow,\                         ;(const ppArrowHead: IVGArrowHead); safecall;
  Get_EndArrow,\                           ;: IVGArrowHead; safecall;
  Set_EndArrow,\                           ;(const ppArrowHead: IVGArrowHead); safecall;
  Get_NibStretch,\                         ;: Integer; safecall;
  Set_NibStretch,\                         ;(pVal: Integer); safecall;
  Get_NibAngle,\                           ;: Double; safecall;
  Set_NibAngle,\                           ;(pVal: Double); safecall;
  Get_BehindFill,\                         ;: WordBool; safecall;
  Set_BehindFill,\                         ;(pVal: WordBool); safecall;
  Get_LineCaps,\                           ;: cdrOutlineLineCaps; safecall;
  Set_LineCaps,\                           ;(pVal: cdrOutlineLineCaps); safecall;
  Get_LineJoin,\                           ;: cdrOutlineLineJoin; safecall;
  Set_LineJoin,\                           ;(pVal: cdrOutlineLineJoin); safecall;
  Get_ScaleWithShape,\                     ;: WordBool; safecall;
  Set_ScaleWithShape,\                     ;(pVal: WordBool); safecall;
  SetProperties,\                          ;(Width: Double; const Style: IVGOutlineStyle; const Color: IVGColor; const StartArrow: IVGArrowHead; const EndArrow: IVGArrowHead; BehindFill: cdrTriState; ScaleWithShape: cdrTriState; LineCaps: cdrOutlineLineCaps; LineJoin: cdrOutlineLineJoin; NibAngle: Double; NibStretch: Integer; DashDotLength: Double; PenWidth: Double; MiterLimit: Double); safecall;
  Get_Size,\                               ;: Double; safecall;
  Set_Size,\                               ;(pVal: Double); safecall;
  GetCopy,\                                ;: IVGOutline; safecall;
  CopyAssign,\                             ;(const SourceOutline: IVGOutline); safecall;
  UserAssign,\                             ;(ParentWindowHandle: Integer): WordBool; safecall;
  Get_PSScreen,\                           ;: IVGPSScreenOptions; safecall;
  CompareWith,\                            ;(const Outline: IVGOutline): WordBool; safecall;
  Get_DashDotLength,\                      ;: Double; safecall;
  Set_DashDotLength,\                      ;(pVal: Double); safecall;
  ToString,\                               ;: WideString; safecall;
  StringAssign,\                           ;(const OutlineString: WideString): WordBool; safecall;
  Get_PenWidth,\                           ;: Double; safecall;
  Set_PenWidth,\                           ;(pVal: Double); safecall;
  Get_MiterLimit,\                         ;: Double; safecall;
  Set_MiterLimit,\                         ;(pVal: Double); safecall;
  SetNoOutline,\                           ; safecall;
  Get_StartArrowOptions,\                  ;: IVGArrowHeadOptions; safecall;
  Set_StartArrowOptions,\                  ;(const ppVal: IVGArrowHeadOptions); safecall;
  Get_EndArrowOptions,\                    ;: IVGArrowHeadOptions; safecall;
  Set_EndArrowOptions,\                    ;(const ppVal: IVGArrowHeadOptions); safecall;
  Get_Justification,\                      ;: cdrOutlineJustification; safecall;
  Set_Justification,\                      ;(pVal: cdrOutlineJustification); safecall;
  SetPropertiesEx                          ;(Width: Double; const Style: IVGOutlineStyle; const Color: IVGColor; const StartArrow: IVGArrowHead; const EndArrow: IVGArrowHead; BehindFill: cdrTriState; ScaleWithShape: cdrTriState; LineCaps: cdrOutlineLineCaps; LineJoin: cdrOutlineLineJoin; NibAngle: Double; NibStretch: Integer; DashDotLength: Double; PenWidth: Double; MiterLimit: Double; Justification: cdrOutlineJustification); safecall;

interface ICUICommandBars,\
  QueryInterface,\
  AddRef,\
  Release,\
  GetTypeInfoCount,\
  GetTypeInfo,\
  GetIDsOfNames,\
  Invoke,\
  Get_Item,\           ; (IndexOrName: OleVariant): ICUICommandBar; safecall;
  Get_Count,\          ;: Integer; safecall;
  Get__NewEnum,\       ;: IUnknown; safecall;
  Add                  ;(const Name: WideString; Position: cuiBarPosition; Temporary: WordBool): ICUICommandBar; safecall;

interface ICUICommandBar,\
  QueryInterface,\
  AddRef,\
  Release,\
  GetTypeInfoCount,\
  GetTypeInfo,\
  GetIDsOfNames,\
  Invoke,\
  Get_type,\           ;: cuiBarType; safecall;
  Get_Visible,\        ;: WordBool; safecall;
  Set_Visible,\        ;(pVal: WordBool); safecall;
  Get_Controls,\       ;: ICUIControls; safecall;
  Get_Modes,\          ;: ICUICommandBarModes; safecall;
  Get_BuiltIn,\        ;: WordBool; safecall;
  Get_Enabled,\        ;: WordBool; safecall;
  Set_Enabled,\        ;(pVal: WordBool); safecall;
  Get_Left,\           ;: Integer; safecall;
  Set_Left,\           ;(pVal: Integer); safecall;
  Get_Top,\            ;: Integer; safecall;
  Set_Top,\            ;(pVal: Integer); safecall;
  Get_Height,\         ;: Integer; safecall;
  Get_Width,\          ;: Integer; safecall;
  Get_Index,\          ;: Integer; safecall;
  Get_Name,\           ;: WideString; safecall;
  Set_Name,\           ;(const pVal: WideString); safecall;
  Get_NameLocal,\      ;: WideString; safecall;
  Set_NameLocal,\      ;(const pVal: WideString); safecall;
  Get_Position,\       ;: cuiBarPosition; safecall;
  Set_Position,\       ;(pVal: cuiBarPosition); safecall;
  Get_Protection,\     ;: cuiBarProtection; safecall;
  Set_Protection,\     ;(pVal: cuiBarProtection); safecall;
  Delete,\             ;; safecall;
  Reset,\              ;; safecall;
  ShowPopup,\          ;(x: OleVariant; y: OleVariant); safecall;
  SetWidth             ;(Width: Integer); safecall;

interface ICUIControls,\
  QueryInterface,\
  AddRef,\
  Release,\
  GetTypeInfoCount,\
  GetTypeInfo,\
  GetIDsOfNames,\
  Invoke,\
  Get_Count,\           ;: Integer; safecall;
  Get_Item,\            ;(Index: Integer): ICUIControl; safecall;
  Get__NewEnum,\        ;: IUnknown; safecall;
  Add,\                 ;(const ControlID: WideString; Index: Integer; Temporary: WordBool): ICUIControl; safecall;
  AddCustomButton,\     ;(const CategoryID: WideString; const Command: WideString; Index: Integer; Temporary: WordBool): ICUIControl; safecall;
  AddCustomControl,\    ;(const ClassName: WideString; const AssemblyPath: WideString; Index: Integer; Temporary: WordBool): ICUIControl; safecall;
  AddToggleButton,\     ;(const Guid: WideString; Index: Integer; Temporary: WordBool): ICUIControl; safecall;
  Remove                ;(Index: Integer); safecall;

interface ICUIControl,\
  QueryInterface,\
  AddRef,\
  Release,\
  GetTypeInfoCount,\
  GetTypeInfo,\
  GetIDsOfNames,\
  Invoke,\
  Get_Caption,\         ;: WideString; safecall;
  Set_Caption,\         ;(const pVal: WideString); safecall;
  Get_DescriptionText,\ ;: WideString; safecall;
  Set_DescriptionText,\ ;(const pVal: WideString); safecall;
  Get_Height,\          ;: Integer; safecall;
  Set_Height,\          ;(pVal: Integer); safecall;
  Get_Width,\           ;: Integer; safecall;
  Set_Width,\           ;(pVal: Integer); safecall;
  Get_ID,\              ;: WideString; safecall;
  Get_Parameter,\       ;: OleVariant; safecall;
  Set_Parameter,\       ;(pVal: OleVariant); safecall;
  Get_Tag,\             ;: WideString; safecall;
  Set_Tag,\             ;(const pVal: WideString); safecall;
  Get_ToolTipText,\     ;: WideString; safecall;
  Set_ToolTipText,\     ;(const pVal: WideString); safecall;
  Get_Visible,\         ;: WordBool; safecall;
  Set_Visible,\         ;(pVal: WordBool); safecall;
  SetIcon,\             ;(RowIndex: Integer; ColumnIndex: Integer); safecall;
  SetCustomIcon,\       ;(const ImageFile: WideString); safecall;
  SetIcon2              ;(const Icon: WideString); safecall;

interface IVGGMSManager,\
  QueryInterface,\
  AddRef,\
  Release,\
  GetTypeInfoCount,\
  GetTypeInfo,\
  GetIDsOfNames,\
  Invoke,\
  Get_GMSPath,\            ;: WideString; safecall;
  RunMacro,\               ;(const ModuleName: WideString; const MacroName: WideString; var Parameters: pointer; Result: pointer): dword; stdcall;
  Get_UserGMSPath,\        ;: WideString; safecall;
  Get_Projects             ;: IVGGMSProjects; safecall;

interface ICUIApplication,\
  QueryInterface,\
  AddRef,\
  Release,\
  GetTypeInfoCount,\
  GetTypeInfo,\
  GetIDsOfNames,\
  Invoke,\
  Get_DataContext,\        ;: ICUIDataContext; safecall;
  RegisterDataSource,\     ; (const DataSourceName: WideString; const Factory: ICUIDataSourceFactory; const CategoryList: WideString; AutoCreateInstance: WordBool): WordBool; safecall;
  UnregisterDataSource,\   ;(const DataSourceName: WideString): WordBool; safecall;
  CreateImageList,\        ;: ICUIImageList; safecall;
  Get_FrameWork,\          ;: ICUIFrameWork; safecall;
  CreateScreenRect,\       ;(Left: Integer; Top: Integer; Width: Integer; Height: Integer): ICUIScreenRect; safecall;
  CreateBitmapImage,\      ;(ImageData: OleVariant; MaxSize: Integer): ICUIBitmapImage; safecall;
  CreateStatusText,\       ;: ICUIStatusText; safecall;
  LoadLocalizedString,\    ;(const Guid: WideString): WideString; safecall;
  DataContext,\            ;: ICUIDataContext read Get_DataContext;
  FrameWork                ;: ICUIFrameWork read Get_FrameWork;

interface ICUIDataContext,\
  QueryInterface,\
  AddRef,\
  Release,\
  GetTypeInfoCount,\
  GetTypeInfo,\
  GetIDsOfNames,\
  Invoke,\
  CreateChildDataContext,\ ;(const CategoryList: WideString): ICUIDataContext; safecall;
  Get_Categories,\         ;: WideString; safecall;
  HasCategory,\            ;(const Category: WideString): WordBool; safecall;
  AddDataSource,\          ;(const DataSourceName: WideString; const DataSourceObject: IDispatch): ICUIDataSourceProxy; safecall;
  ShowDialog,\             ;(const dialogID: WideString): WordBool; safecall;
  GetDataSource            ;(const DataSourceName: WideString): ICUIDataSourceProxy; safecall;

interface ICUIDataSourceProxy,\
  QueryInterface,\
  AddRef,\
  Release,\
  GetTypeInfoCount,\
  GetTypeInfo,\
  GetIDsOfNames,\
  Invoke,\
  UpdateListeners,\        ;(const ListenerNames: WideString); safecall;
  Get_Application,\        ;: ICUIApplication; safecall;
  Get_Name,\               ;: WideString; safecall;
  InvokeMethod,\           ;(const MethodName: WideString); safecall;
  GetProperty,\            ;(const PropertyName: WideString): OleVariant; safecall;
  SetProperty              ;(const PropertyName: WideString; Value: OleVariant); safecall;

;                      dd 72
;cdrCmdCategoryMacros  du '2cc24a3e-fe24-4708-9a74-9c75406eebcd',0
                      dd 72
cdrCmdCategoryPlugins du 'ab489730-8791-45d2-a825-b78bbe0d6a5d',0