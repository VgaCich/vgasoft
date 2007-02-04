unit J2000Dec;
(*
 * Copyright (c) 2001-2002, David Janssens
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS `AS IS'
 * AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
 * LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
 * CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
 * SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
 * INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
 * CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 * POSSIBILITY OF SUCH DAMAGE.
 *
 * 24/04/2003: memory leak fixes by Steve Williams
 *)
(*
  Removing coding part and adapting for Delphi:
  (c)VgaSoft, 2004-2006
*)
interface

uses
  Windows;

const
  J2kDLL='J2000Dec.dll';

  J2K_MAXRLVLS=33;
  J2K_MAXBANDS=3*J2K_MAXRLVLS+1;

{  J2K_CP_CSTY_PRT=$01;
  J2K_CP_CSTY_SOP=$02;
  J2K_CP_CSTY_EPH=$04;
  J2K_CCP_CSTY_PRT=$01;
  J2K_CCP_CBLKSTY_LAZY=$01;
  J2K_CCP_CBLKSTY_RESET=$02;
  J2K_CCP_CBLKSTY_TERMALL=$04;
  J2K_CCP_CBLKSTY_VSC=$08;
  J2K_CCP_CBLKSTY_PTERM=$10;
  J2K_CCP_CBLKSTY_SEGSYM=$20;
  J2K_CCP_QNTSTY_NOQNT=0;
  J2K_CCP_QNTSTY_SIQNT=1;
  J2K_CCP_QNTSTY_SEQNT=2; }

type
  PIntegerArr=^TIntegerArr;
  TIntegerArr=array[0..(MaxInt div SizeOf(Integer))-1] of Integer;
  PJ2kComp=^TJ2kComp;
  TJ2kComp=record
    dx, dy: Integer; //XRsiz, YRsiz
    Prec: Integer; //Precision
    Sgnd: Integer; //Signed
    Data: PIntegerArr; //Image-component data
  end;
  PJ2kCompArr=^TJ2kCompArr;
  TJ2kCompArr=array[0..(MaxInt div SizeOf(TJ2kComp))-1] of TJ2kComp;
  PJ2kImage=^TJ2kImage;
  TJ2kImage=record
    x0, y0: Integer; //XOsiz, YOsiz
    x1, y1: Integer; //Xsiz, Ysiz
    NumComps: Integer; //Number of components
    Comps: PJ2kCompArr; //Image-components
  end;
  TJ2kStepSize=record
    Expn: Integer; //Exponent
    Mant: Integer; //Mantissa
  end;
  PJ2kTCCP=^TJ2kTCCP;
  TJ2kTCCP=record
    CSty: Integer; //Coding style
    NumResolutions: Integer; //Number of resolutions
    CBlkW: Integer; //Width of code-blocks
    CBlkH: Integer; //Height of code-blocks
    CBlkSty: Integer; //Code-block coding style
    QmfbId: Integer; //Discrete wavelet transform identifier
    QntSty: Integer; //Quantisation style
    StepSizes: array[0..J2K_MAXBANDS-1] of TJ2kStepSize; //Stepsizes used for quantisation
    NumGBits: Integer; //Number of guard bits
    RoIShift: Integer; //Region Of Interest shift
    PrcW: array[0..J2K_MAXRLVLS-1] of Integer; //Precinct width
    PrcH: array[0..J2K_MAXRLVLS-1] of Integer; //Precinct height
  end;
  PJ2kTCCPArr=^TJ2kTCCPArr;
  TJ2kTCCPArr=array[0..(MaxInt div SizeOf(TJ2kTCCP))-1] of TJ2kTCCP;
  TJ2kPOC=record
    ResNo0, CompNo0: Integer;
    LayNo1, ResNo1, CompNo1: Integer;
    Prg: Integer;
  end;
  PJ2kTCP=^TJ2kTCP;
  TJ2kTCP=record
    CSty: Integer; //Coding style
    Prg: Integer; //Progression order
    NumLayers: Integer; //Number of layers
    MCT: Integer; //Multi-component transform identifier
    Rates: array[0..31] of Integer; //Rates of layers
    NumPOCs: Integer; //Number of progression order changes
    POCs: array[0..31] of TJ2kPOC; //Progression order changes
    TCCPs: PJ2kTCCPArr; //Tile-component coding parameters
  end;
  PJ2kTCPArr=^TJ2kTCPArr;
  TJ2kTCPArr=array[0..(MaxInt div SizeOf(TJ2kTCP))-1] of TJ2kTCP;
  PJ2kCP=^TJ2kCP;
  TJ2kCP=record
    tx0, ty0: Integer; //XTOsiz, YTOsiz
    tdx, tdy: Integer; //XTsiz, YTsiz
    tw, th: Integer;
    TCPs: PJ2kTCPArr; //Tile coding parameters
  end;

var
  (*
   * Decode an image from a JPEG-2000 codestream
   * Src: source buffer
   * Len: length of source buffer
   * Img: decode image
   * CP: coding parameters that were used to encode the image
   *)
  j2k_decode: function(Src: Pointer; Len: Integer; var Img: PJ2kImage; var CP: PJ2kCP): Integer; stdcall;

  // After processing a stream decoded with j2k_decode, call j2k_release to clean up.
  j2k_release: procedure(Img: PJ2kImage; CP: PJ2kCP); stdcall;

function J2kInit(Lib: hModule): Boolean;

implementation

function J2kInit(Lib: hModule): Boolean;
begin
  j2k_decode:=GetProcAddress(Lib, 'j2k_decode');
  j2k_release:=GetProcAddress(Lib, 'j2k_release');
  Result:=(@j2k_decode<>nil) and (@j2k_release<>nil);
end;

end.
