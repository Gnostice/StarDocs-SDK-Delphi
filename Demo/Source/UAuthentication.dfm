object frmAuthentication: TfrmAuthentication
  Left = 500
  Top = 381
  Caption = 'Authentication'
  ClientHeight = 192
  ClientWidth = 485
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 8
    Width = 342
    Height = 19
    Caption = 'Please enter the StarDocs API credentials'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Label2: TLabel
    Left = 24
    Top = 84
    Width = 38
    Height = 13
    Caption = 'API Key'
  end
  object Label3: TLabel
    Left = 24
    Top = 120
    Width = 51
    Height = 13
    Caption = 'API Secret'
  end
  object Label4: TLabel
    Left = 24
    Top = 48
    Width = 65
    Height = 13
    Caption = 'StarDocs URL'
  end
  object edtKey: TEdit
    Left = 104
    Top = 81
    Width = 361
    Height = 21
    TabOrder = 0
  end
  object edtSecret: TEdit
    Left = 104
    Top = 117
    Width = 361
    Height = 21
    TabOrder = 1
  end
  object btnOK: TButton
    Left = 304
    Top = 153
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    TabOrder = 2
    OnClick = btnOKClick
  end
  object btnCancel: TButton
    Left = 390
    Top = 153
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    TabOrder = 3
    OnClick = btnCancelClick
  end
  object LinkLabel1: TLinkLabel
    Left = 24
    Top = 153
    Width = 123
    Height = 17
    Caption = 
      'Get your free keys <a href="http://www.gnostice.com/StarDocs.asp' +
      '?show=trial">here</a>!'
    TabOrder = 4
    OnLinkClick = LinkLabel1LinkClick
  end
  object edtURL: TEdit
    Left = 104
    Top = 45
    Width = 361
    Height = 21
    TabOrder = 5
    Text = 'http://api.gnostice.com/stardocs/v1'
  end
end
