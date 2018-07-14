VERSION 5.00
Begin VB.Form Jump 
   AutoRedraw      =   -1  'True
   BorderStyle     =   0  'None
   Caption         =   "Jump"
   ClientHeight    =   9615
   ClientLeft      =   105
   ClientTop       =   105
   ClientWidth     =   13005
   KeyPreview      =   -1  'True
   LinkTopic       =   "Form1"
   ScaleHeight     =   641
   ScaleMode       =   3  'Pixel
   ScaleWidth      =   867
   ShowInTaskbar   =   0   'False
   WindowState     =   2  'Maximized
   Begin VB.CommandButton LoadAI 
      Caption         =   "Loading AI Please Wait"
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   29.25
         Charset         =   0
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   3540
      Left            =   0
      TabIndex        =   22
      Top             =   0
      Visible         =   0   'False
      Width           =   6660
   End
   Begin VB.CommandButton random 
      Caption         =   "Command1"
      Height          =   195
      Left            =   9240
      TabIndex        =   21
      Top             =   1680
      Visible         =   0   'False
      Width           =   375
   End
   Begin VB.Frame Frame1 
      Height          =   1500
      Index           =   2
      Left            =   840
      OLEDropMode     =   1  'Manual
      TabIndex        =   12
      Top             =   0
      Width           =   8040
      Begin VB.TextBox Text 
         Alignment       =   2  'Center
         BackColor       =   &H80000006&
         Enabled         =   0   'False
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   24
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   495
         Index           =   2
         Left            =   1320
         TabIndex        =   20
         Top             =   840
         Width           =   495
      End
      Begin VB.TextBox Text1 
         Alignment       =   2  'Center
         BackColor       =   &H80000006&
         Enabled         =   0   'False
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   24
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   495
         Index           =   2
         Left            =   3240
         TabIndex        =   19
         Top             =   120
         Width           =   495
      End
      Begin VB.TextBox Text2 
         Alignment       =   2  'Center
         BackColor       =   &H80000007&
         Enabled         =   0   'False
         BeginProperty Font 
            Name            =   "Wingdings 3"
            Size            =   26.25
            Charset         =   2
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         ForeColor       =   &H80000010&
         Height          =   615
         Index           =   2
         Left            =   240
         TabIndex        =   18
         Text            =   "ä"
         Top             =   840
         Visible         =   0   'False
         Width           =   615
      End
      Begin VB.TextBox Text3 
         Alignment       =   2  'Center
         BackColor       =   &H80000007&
         Enabled         =   0   'False
         BeginProperty Font 
            Name            =   "Wingdings 3"
            Size            =   26.25
            Charset         =   2
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         ForeColor       =   &H80000010&
         Height          =   615
         Index           =   2
         Left            =   240
         TabIndex        =   17
         Text            =   "ã"
         Top             =   240
         Visible         =   0   'False
         Width           =   615
      End
      Begin VB.TextBox Text4 
         Alignment       =   2  'Center
         BackColor       =   &H80000007&
         Enabled         =   0   'False
         BeginProperty Font 
            Name            =   "Wingdings 3"
            Size            =   26.25
            Charset         =   2
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         ForeColor       =   &H80000010&
         Height          =   615
         Index           =   2
         Left            =   2760
         TabIndex        =   16
         Text            =   "á"
         Top             =   720
         Visible         =   0   'False
         Width           =   615
      End
      Begin VB.TextBox Text5 
         BackColor       =   &H80000007&
         Enabled         =   0   'False
         BeginProperty Font 
            Name            =   "Wingdings 3"
            Size            =   26.25
            Charset         =   2
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         ForeColor       =   &H80000010&
         Height          =   615
         Index           =   2
         Left            =   3600
         TabIndex        =   15
         Text            =   "â"
         Top             =   720
         Visible         =   0   'False
         Width           =   615
      End
      Begin VB.TextBox Text6 
         BackColor       =   &H80000006&
         Enabled         =   0   'False
         Height          =   615
         Index           =   2
         Left            =   5760
         TabIndex        =   14
         Top             =   720
         Visible         =   0   'False
         Width           =   615
      End
      Begin VB.TextBox Text7 
         BackColor       =   &H80000007&
         Enabled         =   0   'False
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   24
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   615
         Index           =   2
         Left            =   4440
         TabIndex        =   13
         Top             =   720
         Width           =   1095
      End
   End
   Begin VB.Frame Frame1 
      Height          =   1500
      Index           =   1
      Left            =   1200
      OLEDropMode     =   1  'Manual
      TabIndex        =   3
      Top             =   2160
      Width           =   8040
      Begin VB.TextBox Text 
         Alignment       =   2  'Center
         BackColor       =   &H80000006&
         Enabled         =   0   'False
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   24
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   495
         Index           =   1
         Left            =   1320
         TabIndex        =   11
         Top             =   840
         Width           =   495
      End
      Begin VB.TextBox Text1 
         Alignment       =   2  'Center
         BackColor       =   &H80000006&
         Enabled         =   0   'False
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   24
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   495
         Index           =   1
         Left            =   3240
         TabIndex        =   10
         Top             =   120
         Width           =   495
      End
      Begin VB.TextBox Text2 
         Alignment       =   2  'Center
         BackColor       =   &H80000007&
         Enabled         =   0   'False
         BeginProperty Font 
            Name            =   "Wingdings 3"
            Size            =   26.25
            Charset         =   2
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         ForeColor       =   &H80000010&
         Height          =   615
         Index           =   1
         Left            =   240
         TabIndex        =   9
         Text            =   "ä"
         Top             =   840
         Visible         =   0   'False
         Width           =   615
      End
      Begin VB.TextBox Text3 
         Alignment       =   2  'Center
         BackColor       =   &H80000007&
         Enabled         =   0   'False
         BeginProperty Font 
            Name            =   "Wingdings 3"
            Size            =   26.25
            Charset         =   2
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         ForeColor       =   &H80000010&
         Height          =   615
         Index           =   1
         Left            =   240
         TabIndex        =   8
         Text            =   "ã"
         Top             =   240
         Visible         =   0   'False
         Width           =   615
      End
      Begin VB.TextBox Text4 
         Alignment       =   2  'Center
         BackColor       =   &H80000007&
         Enabled         =   0   'False
         BeginProperty Font 
            Name            =   "Wingdings 3"
            Size            =   26.25
            Charset         =   2
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         ForeColor       =   &H80000010&
         Height          =   615
         Index           =   1
         Left            =   2760
         TabIndex        =   7
         Text            =   "á"
         Top             =   720
         Visible         =   0   'False
         Width           =   615
      End
      Begin VB.TextBox Text5 
         BackColor       =   &H80000007&
         Enabled         =   0   'False
         BeginProperty Font 
            Name            =   "Wingdings 3"
            Size            =   26.25
            Charset         =   2
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         ForeColor       =   &H80000010&
         Height          =   615
         Index           =   1
         Left            =   3600
         TabIndex        =   6
         Text            =   "â"
         Top             =   720
         Visible         =   0   'False
         Width           =   615
      End
      Begin VB.TextBox Text6 
         BackColor       =   &H80000006&
         Enabled         =   0   'False
         Height          =   615
         Index           =   1
         Left            =   5640
         TabIndex        =   5
         Top             =   720
         Visible         =   0   'False
         Width           =   615
      End
      Begin VB.TextBox Text7 
         BackColor       =   &H80000007&
         Enabled         =   0   'False
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   24
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   615
         Index           =   1
         Left            =   4440
         TabIndex        =   4
         Top             =   720
         Width           =   1095
      End
   End
   Begin VB.CommandButton StartScreen 
      Caption         =   "Click to go to Menu and Reset Score"
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   24
         Charset         =   0
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   1500
      Left            =   7200
      TabIndex        =   1
      Top             =   6000
      Width           =   9120
   End
   Begin VB.PictureBox Picture1 
      Enabled         =   0   'False
      Height          =   2895
      Left            =   -120
      Picture         =   "new jump.frx":0000
      ScaleHeight     =   2835
      ScaleWidth      =   3915
      TabIndex        =   0
      Top             =   840
      Visible         =   0   'False
      Width           =   3975
   End
   Begin VB.CommandButton AI 
      Caption         =   "jump AI"
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   24
         Charset         =   0
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   1500
      Left            =   0
      TabIndex        =   2
      Top             =   7440
      Width           =   7215
   End
   Begin VB.Shape Command 
      BackStyle       =   1  'Opaque
      BorderStyle     =   3  'Dot
      Height          =   150
      Index           =   2
      Left            =   6240
      Top             =   1440
      Width           =   150
   End
   Begin VB.Shape Command 
      BackColor       =   &H00FFFFFF&
      BackStyle       =   1  'Opaque
      BorderStyle     =   3  'Dot
      FillColor       =   &H00FFFFFF&
      Height          =   150
      Index           =   1
      Left            =   7920
      Top             =   2160
      Width           =   150
   End
End
Attribute VB_Name = "Jump"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
DefInt A-Z
Dim score(2), flag$(2), oldupdown(2), oldrightleft(2), updown(2), rightleft(2)
Dim p As Integer, oldp As Integer, pot As Integer
Dim greenx, greeny, bluex, bluey, xx, yy As Integer
Private Declare Function Beep Lib "kernel32" (ByVal dwFreq As Long, ByVal dwDuration As Long) As Long
Dim left1 As Boolean, right1 As Boolean, top1 As Boolean, down1 As Boolean, leftright As Integer
Dim avoidleft(2) As Boolean, avoidright(2) As Boolean, avoidtop(2) As Boolean, avoiddown(2) As Boolean
Dim pathx(1000, 1000), pathy(1000, 1000), movex As Integer, movey As Integer
Dim glitch As Integer, choosev As Integer, choosev1 As Integer, test As Integer, path As Integer, short As Integer, shortestchoose As Integer
Dim udmax(2), rlmax(2), udmin(2), rlmin(2), cleft As Integer, ctop As Integer
Dim shorty As Integer, shortestchoosey As Integer, testx As Integer
Dim rL As Integer, ud As Integer, differancey As Integer, differancex As Integer
Dim dontdothis As Boolean, wait As Integer, moveon As Boolean
Dim methodavoid As Integer, moveonavoid As Boolean, depdown As Boolean, derightleft As Boolean, n
Dim xwait As Boolean, canplay As Boolean, ai2loop As Integer
Dim fakecleft As Integer, fakectop As Integer, fakemovex As Integer, fakemovey As Integer, fakeud As Integer, fakerl As Integer
Dim baknum As Integer, ai1(2) As Boolean, ai2(2) As Boolean, shortestwaitx As Integer, shortestwaity As Integer
Dim way(3, 40), trynumberavoid As Integer, checkavoid As Integer, checkpath As Integer, numberavoid As Integer
Dim dontdoavoid As Boolean, avoidpath As Integer, avoidshort As Integer, avoidpathshort As Integer, avoidpathshort1 As Integer, usingavoid1 As Boolean
Dim trycleft As Integer, tryctop As Integer, trymovex As Integer, trymovey As Integer
Dim rlvel As Integer, udvel As Integer, rlcount As Integer, udcount As Integer, differance As Integer
Dim pathru As Integer, pathrutry As Integer, pathrl As Integer, pathud As Integer, pathrud As Integer
Dim bluemovex As Integer, bluemovey As Integer, cmdleft As Integer, cmdtop As Integer
Dim bleu As Integer, vert As Integer, blgr As Integer, shortestchooseblue As Integer, shortestchoosebluey As Integer, pathbluex(1000), pathbluey(1000)

Private Sub Form_Load()
Cls
Picture1.Visible = False
AI.Visible = True
For p = 1 To 2
    score(p) = 0
Next
End Sub

Private Sub StartScreen_Click()
Unload Jump
Load Form1
Form1.Show vbModal
End Sub

Private Sub ai_Click()

start:
Cls
Randomize Timer
Me.BackColor = RGB(0, 0, 0)
LoadAI.Width = Jump.ScaleWidth
LoadAI.Height = Jump.ScaleHeight
LoadAI.Visible = True
xx = 0
yy = 0
bluex = 0
bluey = 0
greenx = 0
greeny = 0
bluemovex = 0
bluemovey = 0
For Row = 0 To (Jump.ScaleHeight - 100) Step 10
    For Col = 0 To Jump.ScaleWidth Step 10
        PSet (Col, Row), RGB(255, 255, 255)
    Next
Next
For box = 1 To 52
    Do
        ro = Rnd * (Jump.ScaleHeight - 160)
        co = Rnd * (Jump.ScaleWidth - 60)
    Loop Until Point(co, ro) = RGB(255, 255, 255)
    If box = 51 Then
        Line (co + 20, ro + 20)-Step(40, 40), RGB(0, 255, 0), BF
        greenx = co + 35
        greeny = ro + 35
    ElseIf box = 52 Then
        Line (co + 20, ro + 20)-Step(40, 40), RGB(0, 0, 255), BF
        bluex = co + 35
        bluey = ro + 35
    Else
        Line (co + 20, ro + 20)-Step(40, 40), RGB(255, 0, 0), BF
    End If
Next
For p = 1 To 2
    Do
        ro = Rnd * (Jump.ScaleHeight - 105)
        co = Rnd * Jump.ScaleWidth
    Loop Until Point(co, ro) = RGB(255, 255, 255)
    oldp = 2
    Command(p).Top = ro - 5
    Command(p).Left = co - 5
    Command(p).BackColor = RGB(255, p * 255 - 255 + 50, oldp * 50 - 50)
    updown(p) = 0
    rightleft(p) = 0
    oldupdown(p) = updown(p)
    oldrightleft(p) = rightleft(p)
    flag$(p) = "no"
    Frame1(p).Top = (Jump.ScaleHeight - 100)
    Frame1(p).Left = p * 608 - 607
    Frame1(p).BackColor = RGB(0, 0, 0)
    Text7(p) = score(p)
    Text(p) = 0
    Text1(p) = 0
    Text6(p).BackColor = RGB(0, 0, 0)
    udmax(p) = 1
    rlmax(p) = 1
    udmin(p) = -1
    rlmin(p) = -1
    ai1(p) = 0
    ai2(p) = 0
Next
Line (0, 0)-Step(Jump.ScaleWidth - 1, Jump.ScaleHeight - 2), RGB(255, 255, 255), B
StartScreen.Visible = False
AI.Visible = False
oldp = 2
p = 1
pot = 0
random.Top = Jump.ScaleHeight - 100
random.Left = 0
StartScreen.Left = 608
StartScreen.Top = Jump.ScaleHeight - 200
random.Visible = False
Picture1.Visible = False
starter = 0
shortestchoose = 0
shortestchoosey = 0
shortestchooseblue = 0
shortestchoosegreen = 0
shortestchoosebluey = 0
shortestchoosegreeny = 0
For fun = 1 To 1000
    pathbluex(fun) = 0
    pathbluey(fun) = 0
Next
cmdleft = 0
cmdtop = 0
canplay = False
Do
    starter = starter + 1
    If starter = 1 Then flag$(2) = "no"
    If starter = 2 Then flag$(2) = "yes"
        If cmdleft = 0 Then
            cmdleft = Command(2).Left
            cmdtop = Command(2).Top
        End If
        If shortestchoose = 0 Or shortestchoosey = 0 Then
            If flag$(2) = "no" Then
                xx = bluex
                yy = bluey
                blgr = 1
                bleu = 255
                vert = 0
                For fun2 = 1 To 1000
                    For fun = 1 To 1000
                        pathx(fun, fun2) = 0
                        pathy(fun, fun2) = 0
                    Next
                Next
            ElseIf flag$(2) = "yes" Then
                For fun2 = 1 To 1000
                    For fun = 1 To 1000
                        pathx(fun, fun2) = 0
                        pathy(fun, fun2) = 0
                    Next
                Next
                shortestchoose = 0
                shortestchoosey = 0
                blgr = 2
                bleu = 0
                vert = 255
                xx = greenx
                yy = greeny
            End If
            If ai2(blgr) = False Then
                choosev = 0
                shorty = 0
                short = 0
                differancey = 0
                differancex = 0
                dontdothis = False
                Do
                    cleft = cmdleft
                    choosev = choosev + 1
                    test = 0
                    path = 0
                    dontdothis = False
                    rL = 0
                    ctop = cmdtop
                    movex = 1
                    movey = 1
                    left1 = False
                    right1 = False
                    Do
                        If cleft > xx Then
                            If test < choosev Then
                                rL = rL - 1
                                pathx(choosev, movex) = -1
                                left1 = True
                                test = test + 1
                            End If
                        ElseIf cleft < xx Then
                            If test < choosev Then
                                rL = rL + 1
                                pathx(choosev, movex) = 1
                                test = test + 1
                                right1 = True
                            End If
                        End If
                        path = path + 1
                        cleft = cleft + (rL * 10)
                        movex = movex + 1
                        If cleft < xx And left1 = True And Point(cleft + 5, yy + 5) <> RGB(0, vert, bleu) Then dontdothis = True
                        If cleft > xx And right1 = True And Point(cleft + 5, yy + 5) <> RGB(0, vert, bleu) Then dontdothis = True
                        If movex > 9000 Then dontdothis = True
                    Loop Until dontdothis = True Or Point(cleft + 5, yy + 5) = RGB(0, vert, bleu)
                    If dontdothis = False And flag$(2) <> "yes" Then
                        Do
                            If rL > 0 Then
                                rL = rL - 1
                            ElseIf rL < 0 Then
                                rL = rL + 1
                            End If
                            cleft = cleft + (rL * 10)
                            If cleft < 0 Or cleft > Jump.ScaleWidth Then
                                dontdothis = True
                            End If
                        Loop Until rL = 0 Or dontdothis = True
                    End If
                    If path < short Or short = 0 Then
                        If dontdothis = False And path <> 0 Then
                            short = path
                            shortestchoose = choosev
                        End If
                    End If
                    movey = 1
                    movex = 1
                    top1 = False
                    down1 = False
                    ctop = cmdtop
                    cleft = cmdleft
                    ud = 0
                    path = 0
                    rL = 0
                    dontdothis = False
                    test = 0
                    Do
                        If ctop > yy Then
                            If test < choosev Then
                                ud = ud - 1
                                pathy(choosev, movey) = -1
                                top1 = True
                                test = test + 1
                            End If
                        ElseIf ctop < yy Then
                            If test < choosev Then
                                ud = ud + 1
                                pathy(choosev, movey) = 1
                                down1 = True
                                test = test + 1
                            End If
                        End If
                        path = path + 1
                        ctop = ctop + (ud * 10)
                        movey = movey + 1
                        If ctop < yy And top1 = True And Point(xx + 5, ctop + 5) <> RGB(0, vert, bleu) Then dontdothis = True
                        If ctop > yy And down1 = True And Point(xx + 5, ctop + 5) <> RGB(0, vert, bleu) Then dontdothis = True
                        If movey > 9000 Then dontdothis = True
                    Loop Until dontdothis = True Or Point(xx + 5, ctop + 5) = RGB(0, vert, bleu)
                    If dontdothis = False Then
                        Do
                            If ud > 0 Then
                                ud = ud - 1
                            ElseIf ud < 0 Then
                                ud = ud + 1
                            End If
                            ctop = ctop + (ud * 10)
                            If ctop < 0 Or ctop > (Jump.ScaleHeight - 105) Then
                                dontdothis = True
                            End If
                        Loop Until ud = 0 Or dontdothis = True
                    End If
                    If path < shorty Or shorty = 0 Then
                        If dontdothis = False And path <> 0 Then
                            shorty = path
                            shortestchoosey = choosev
                        End If
                    End If
                Loop Until choosev = 20
                If shorty < short Then
                    differancey = short - shorty
                    For fun = 100 To 1 Step -1
                        pathy(shortestchoosey, fun + differancey) = pathy(shortestchoosey, fun)
                    Next
                    For fun = 1 To differancey
                        pathy(shortestchoosey, fun) = 0
                    Next
                ElseIf shorty > short Then
                    differancex = shorty - short
                    For fun = 100 To 1 Step -1
                        pathx(shortestchoose, fun + differancex) = pathx(shortestchoose, fun)
                    Next
                    For fun = 1 To differancex
                        pathx(shortestchoose, fun) = 0
                    Next
                End If
                movex = 1
                movey = 1
                cleft = cmdleft
                ctop = cmdtop
                ud = 0
                rL = 0
                dontdothis = False
                Do
                    rL = rL + pathx(shortestchoose, movex)
                    ud = ud + pathy(shortestchoosey, movey)
                    movex = movex + 1
                    movey = movey + 1
                    If Point(cleft + (rL * 10) + 5, ctop + (ud * 10) + 5) <> RGB(255, 0, 0) Then
                        cleft = cleft + (rL * 10)
                        ctop = ctop + (ud * 10)
                    ElseIf Point(cleft + (rL * 10) + 5, ctop + (ud * 10) + 5) = RGB(255, 0, 0) Then
                        dontdothis = True
                    End If
                Loop Until Point(cleft + 5, ctop + 5) = RGB(0, vert, bleu) Or dontdothis = True
                If dontdothis = False And flag$(2) <> "yes" Then
                    fakerl = rL
                    fakeud = ud
                    fakectop = ctop
                    fakecleft = cleft
                    fakemovex = movex
                    fakemovey = movey
                    Do
                        If fakerl > 0 Then
                            fakerl = fakerl - 1
                        ElseIf fakerl < 0 Then
                            fakerl = fakerl + 1
                        End If
                        If fakeud > 0 Then
                            fakeud = fakeud - 1
                        ElseIf fakeud < 0 Then
                            fakeud = fakeud + 1
                        End If
                        If fakectop < 0 Or fakectop > (Jump.ScaleHeight - 105) Then dontdothis = True
                        If fakecleft < 0 Or fakecleft > Jump.ScaleWidth Then dontdothis = True
                        If Point(cleft + 5 + fakerl * 10, ctop + 5 + fakeud * 10) <> RGB(255, 0, 0) Then
                            fakectop = fakectop + (fakeud * 10)
                            fakemovey = fakemovey + 1
                            fakecleft = fakecleft + (fakerl * 10)
                            fakemovex = fakemovex + 1
                        Else
                            dontdothis = True
                        End If
                        If fakerl = 0 And fakeud = 0 Then moveon = True
                    Loop Until moveon = True Or dontdothis = True
                End If
                If dontdothis = False And flag$(2) <> "yes" Then
                    Do
                        If rL > 0 Then
                            rL = rL - 1
                            pathx(shortestchoose, movex) = -1
                        ElseIf rL < 0 Then
                            pathx(shortestchoose, movex) = 1
                            rL = rL + 1
                        End If
                        If ud > 0 Then
                            ud = ud - 1
                            pathy(shortestchoosey, movey) = -1
                        ElseIf ud < 0 Then
                            pathy(shortestchoosey, movey) = 1
                            ud = ud + 1
                        End If
                        ctop = ctop + (ud * 10)
                        movey = movey + 1
                        cleft = cleft + (rL * 10)
                        movex = movex + 1
                    Loop Until rL = 0 And ud = 0
                ElseIf dontdothis = True Then
                    If shorty < short Then
                        For fun = 1 To 100
                            pathy(shortestchoosey, fun) = pathy(shortestchoosey, fun + differancey)
                        Next
                    ElseIf shorty > short Then
                        For fun = 1 To 100
                            pathx(shortestchoose, fun) = pathx(shortestchoose, fun + differancex)
                        Next
                    End If
                    ai1(blgr) = True
                    shortestchoose = 0
                    shortestchoosey = 0
                    short = 0
                End If
                If ai1(blgr) = True Then
                    methodavoid = 1
                    checkcheck = 0
                    moveonavoid = False
                    shortestwaitx = 0
                    shortestwaity = 0
                    short = 0
                    Do
                        xwait = False
                        moveon = False
                        Do
                            choosev = 0
                            Do
                                choosev = choosev + 1
                                choosev1 = 0
                                Do
                                    choosev1 = choosev1 + 1
                                    wait = 0
                                    Do
                                        test = 0
                                        testx = 0
                                        movex = 1
                                        movey = 1
                                        ud = 0
                                        rL = 0
                                        cleft = cmdleft
                                        ctop = cmdtop
                                        dontdothis = False
                                        path = 0
                                        pathrl = 0
                                        pathud = 0
                                        pathru = 0
                                        pathrud = 0
                                        Do
                                            If test < wait And xwait = False Then
                                                ud = 0
                                                test = test + 1
                                            Else
                                                ud = ud + pathy(choosev1, movey)
                                                movey = movey + 1
                                            End If
                                            If testx < wait And xwait = True Then
                                                rL = 0
                                                testx = testx + 1
                                            Else
                                                rL = rL + pathx(choosev, movex)
                                                movex = movex + 1
                                            End If
                                            If methodavoid = 2 Then
                                                If Point(cleft + (rL * 10) + 5, ctop + (ud * 10) + 5) = RGB(255, 0, 0) Then
                                                    If Point(cleft + (rL * 10) + 5, ctop + 5) = RGB(255, 0, 0) Then
                                                        If Point(cleft + 5 + (rL * 10), ctop + 5 + ((ud - 1) * 10)) <> RGB(255, 0, 0) Then
                                                            ud = ud - 1
                                                            If pathud <> -2 Then
                                                                pathud = -1
                                                            End If
                                                        ElseIf Point(cleft + 5 + (rL * 10), ctop + 5 + ((ud - 1) * 10)) = RGB(255, 0, 0) Then
                                                            If Point(cleft + 5 + ((rL - pathx(choosev, movex)) * 10), ctop + 5 + ((ud - pathy(choosev1, movey) - 1) * 10)) <> RGB(255, 0, 0) Then
                                                                rL = rL - pathx(choosev, movex)
                                                                movex = movex - 1
                                                                ud = ud - pathy(choosev1, movey)
                                                                movey = movey - 1
                                                                ud = ud - 1
                                                                pathud = -2
                                                            ElseIf Point(cleft + 5 + ((rL - pathx(choosev, movex)) * 10), ctop + 5 + ((ud - pathy(choosev1, movey) - 1) * 10)) = RGB(255, 0, 0) Then
                                                                If Point(cleft + 5 + (rL * 10), ctop + 5 + ((ud + 1) * 10)) <> RGB(255, 0, 0) Then
                                                                    ud = ud + 1
                                                                    If pathud <> 2 Then
                                                                        pathud = 1
                                                                    End If
                                                                ElseIf Point(cleft + 5 + (rL * 10), ctop + 5 + ((ud + 1) * 10)) = RGB(255, 0, 0) Then
                                                                    If Point(cleft + 5 + ((rL - pathx(choosev, movex)) * 10), ctop + 5 + ((ud - pathy(choosev1, movey) + 1) * 10)) <> RGB(255, 0, 0) Then
                                                                        rL = rL - pathx(choosev, movex)
                                                                        movex = movex - 1
                                                                        ud = ud - pathy(choosev1, movey)
                                                                        movey = movey - 1
                                                                        ud = ud + 1
                                                                        pathud = 2
                                                                    End If
                                                                End If
                                                            End If
                                                        End If
                                                    ElseIf Point(cleft + 5, ctop + 5 + (ud * 10)) = RGB(255, 0, 0) Then
                                                        If Point(cleft + 5 + ((rL - 1) * 10), ctop + 5 + (ud * 10)) <> RGB(255, 0, 0) Then
                                                            rL = rL - 1
                                                            If pathrl <> -2 Then
                                                                pathrl = -1
                                                            End If
                                                        ElseIf Point(cleft + 5 + ((rL - 1) * 10), ctop + 5 + (ud * 10)) = RGB(255, 0, 0) Then
                                                            If Point(cleft + 5 + ((rL - pathx(choosev, movex) - 1) * 10), ctop + 5 + ((ud - pathy(choosev1, movey)) * 10)) <> RGB(255, 0, 0) Then
                                                                rL = rL - pathx(choosev, movex)
                                                                movex = movex - 1
                                                                ud = ud - pathy(choosev1, movey)
                                                                movey = movey - 1
                                                                rL = rL - 1
                                                                pathrl = -2
                                                            ElseIf Point(cleft + 5 + ((rL - pathx(choosev, movex) - 1) * 10), ctop + 5 + ((ud - pathy(choosev1, movey)) * 10)) = RGB(255, 0, 0) Then
                                                                If Point(cleft + 5 + ((rL + 1) * 10), ctop + 5 + (ud * 10)) <> RGB(255, 0, 0) Then
                                                                    rL = rL + 1
                                                                    If pathrl <> 2 Then
                                                                        pathrl = 1
                                                                    End If
                                                                ElseIf Point(cleft + 5 + ((rL + 1) * 10), ctop + 5 + (ud * 10)) = RGB(255, 0, 0) Then
                                                                    If Point(cleft + 5 + ((rL - pathx(choosev, movex) + 1) * 10), ctop + 5 + ((ud - pathy(choosev1, movey)) * 10)) <> RGB(255, 0, 0) Then
                                                                        rL = rL - pathx(choosev, movex)
                                                                        movex = movex - 1
                                                                        ud = ud - pathy(choosev1, movey)
                                                                        movey = movey - 1
                                                                        rL = rL + 1
                                                                        pathrl = 2
                                                                    End If
                                                                End If
                                                            End If
                                                        End If
                                                    End If
                                                End If
                                            End If
                                            If path = 100 Then dontdothis = True
                                            If methoavoid <> 1 Then pathru = pathru + 1
                                            If Point(cleft + (rL * 10) + 5, ctop + (ud * 10) + 5) <> RGB(255, 0, 0) Then
                                                If flag$(2) <> "yes" Then
                                                    If pathrl = 2 Then
                                                        pathru = pathru + 1
                                                        If pathru >= 3 And Point(cleft + (rL * 10) + 5 - 10, ctop + 5 + (ud * 10) + 5) <> RGB(255, 0, 0) Then
                                                            rL = rL + 1
                                                            pathrl = 1
                                                        End If
                                                    ElseIf pathrl = 1 Then
                                                        pathru = pathru + 1
                                                        If pathru >= 2 And Point(cleft + (rL * 10) + 5 - 10, ctop + 5 + (ud * 10) + 5) <> RGB(255, 0, 0) Then
                                                            rL = rL + 1
                                                            pathrl = 0
                                                            pathru = 0
                                                        End If
                                                    End If
                                                    If pathrl = -2 Then
                                                        pathru = pathru + 1
                                                        If pathru >= 3 And Point(cleft + (rL * 10) + 5 + 10, ctop + 5 + (ud * 10) + 5) <> RGB(255, 0, 0) Then
                                                            rL = rL - 1
                                                            pathrl = -1
                                                        End If
                                                    ElseIf pathrl = -1 Then
                                                        pathru = pathru + 1
                                                        If pathru >= 2 And Point(cleft + (rL * 10) + 5 + 10, ctop + 5 + (ud * 10) + 5) <> RGB(255, 0, 0) Then
                                                            rL = rL - 1
                                                            pathrl = 0
                                                            pathru = 0
                                                        End If
                                                    End If
                                                    If pathud = 2 Then
                                                        pathrud = pathrud + 1
                                                        If pathrud >= 3 And Point(cleft + (rL * 10) + 5, ctop + 5 + (ud * 10) + 5 - 10) <> RGB(255, 0, 0) Then
                                                            ud = ud + 1
                                                            pathud = 1
                                                        End If
                                                    ElseIf pathud = 1 Then
                                                        pathrud = pathrud + 1
                                                        If pathrud >= 2 And Point(cleft + (rL * 10) + 5, ctop + 5 + (ud * 10) + 5 - 10) <> RGB(255, 0, 0) Then
                                                            ud = ud + 1
                                                            pathud = 0
                                                            pathrud = 0
                                                        End If
                                                    End If
                                                    If pathud = -2 Then
                                                        pathrud = pathrud + 1
                                                        If pathrud >= 3 And Point(cleft + (rL * 10) + 5, ctop + 5 + (ud * 10) + 5 + 10) <> RGB(255, 0, 0) Then
                                                            ud = ud - 1
                                                            pathud = -1
                                                        End If
                                                    ElseIf pathud = -1 Then
                                                        pathrud = pathrud + 1
                                                        If pathrud >= 2 And Point(cleft + (rL * 10) + 5, ctop + 5 + (ud * 10) + 5 + 10) <> RGB(255, 0, 0) Then
                                                            ud = ud - 1
                                                            pathud = 0
                                                            pathrud = 0
                                                        End If
                                                    End If
                                                End If
                                                cleft = cleft + (rL * 10)
                                                ctop = ctop + (ud * 10)
                                                path = path + 1
                                            ElseIf Point(cleft + (rL * 10) + 5, ctop + (ud * 10) + 5) = RGB(255, 0, 0) Then
                                                dontdothis = True
                                            End If
                                            If ctop < yy And ud < 0 And Point(cleft + 5, ctop + 5) <> RGB(0, vert, bleu) Then dontdothis = True
                                            If ctop > yy And ud > 0 And Point(cleft + 5, ctop + 5) <> RGB(0, vert, bleu) Then dontdothis = True
                                            If cleft < xx And rL < 0 And Point(cleft + 5, ctop + 5) <> RGB(0, vert, bleu) Then dontdothis = True
                                            If cleft > xx And rL > 0 And Point(cleft + 5, ctop + 5) <> RGB(0, vert, bleu) Then dontdothis = True
                                            If Point(cleft + 5, ctop + 5) = RGB(0, vert, bleu) And methodavoid <> 1 Then
                                            End If
                                            If movey = 0 Or movex = 0 Then dontdothis = True
                                        Loop Until dontdothis = True Or Point(cleft + 5, ctop + 5) = RGB(0, vert, bleu)
                                        If methodavoid = 2 Then
                                            pathrl = rL
                                            pathud = ud
                                            End If
                                            If dontdothis = False And flag$(2) <> "yes" Then
                                                Do
                                                    If rL > 0 Then
                                                        rL = rL - 1
                                                    ElseIf rL < 0 Then
                                                        rL = rL + 1
                                                    End If
                                                    If ud > 0 Then
                                                        ud = ud - 1
                                                    ElseIf ud < 0 Then
                                                        ud = ud + 1
                                                    End If
                                                    If ctop < 0 Or ctop > (Jump.ScaleHeight - 105) Then
                                                        dontdothis = True
                                                    End If
                                                    If cleft < 0 Or cleft > Jump.ScaleWidth Then
                                                        dontdothis = True
                                                    End If
                                                    If Point(cleft + 5 + rL * 10, ctop + 5 + ud * 10) = RGB(255, 0, 0) Then dontdothis = True
                                                    ctop = ctop + (ud * 10)
                                                    movey = movey + 1
                                                    cleft = cleft + (rL * 10)
                                                    movex = movex + 1
                                                Loop Until dontdothis = True Or (rL = 0 And ud = 0)
                                            End If
                                            If path < short Or short = 0 Then
                                                If dontdothis = False And path <> 0 And wait <> 0 Then
                                                    short = path
                                                    shortestchoosey = choosev1
                                                    shortestchoose = choosev
                                                    If xwait = True Then
                                                        shortestwaitx = wait
                                                    Else
                                                        shortestwaity = wait
                                                    End If
                                                End If
                                            End If
                                            wait = wait + 1
                                        Loop Until wait = 10
                                    Loop Until choosev1 = 20
                                Loop Until choosev = 20
                        If xwait = True Then moveon = True
                        If shortestchoose = 0 And shortestchoosey = 0 Then
                            xwait = True
                        Else
                            xwait = False
                        End If
                        Loop Until xwait = False Or moveon = True
                        If methodavoid = 2 Then moveonavoid = True
                        If shortestchoose = 0 And shortestchoosey = 0 Then
                            methodavoid = methodavoid + 1
                        Else
                            moveonavoid = True
                        End If
                        Loop Until moveonavoid = True
                    If shortestwaity <> 0 Then
                        For fun = (short + 1) To 1 Step -1
                            pathy(shortestchoosey, fun + shortestwaity) = pathy(shortestchoosey, fun)
                        Next
                        For fun = 1 To shortestwaity
                            pathy(shortestchoosey, fun) = 0
                        Next
                    End If
                    If shortestwaitx <> 0 Then
                        For fun = (short + 1) To 1 Step -1
                            pathx(shortestchoose, fun + shortestwaitx) = pathx(shortestchoose, fun)
                        Next
                        For fun = 1 To shortestwaitx
                            pathx(shortestchoose, fun) = 0
                        Next
                    End If
                    If short <> 0 Then
                    movex = 1
                    movey = 1
                    rL = 0
                    ud = 0
                    cleft = cmdleft
                    ctop = cmdtop
                    
                    
                    If methodavoid = 2 Then
                    pathrl = 0
                    pathru = 0
                    pathud = 0
                    pathrud = 0
                    numb = 1
                    Do
                        rL = rL + pathx(shortestchoose, movex)
                        ud = ud + pathy(shortestchoosey, movey)
                        If movex = 1000 Or movey = 1000 Then dontdothis = True
                        If cleft > 10000 Or cleft < 10000 Then
                            movex = 1
                            movey = 1
                            cleft = cmdleft
                            ctop = cmdtop
                            If numb = 1 Then
                                numb = -1
                            ElseIf numb = -1 Then
                                numb = 0
                            ElseIf numb = 0 Then
                                dontdothis = True
                            End If
                        End If
                        If Point(cleft + (rL * 10) + 5, ctop + (ud * 10) + 5) = RGB(255, 0, 0) Then
                                            
                                            If Point(cleft + (rL * 10) + 5, ctop + 5) = RGB(255, 0, 0) Then
                                                If Point(cleft + 5 + (rL * 10), ctop + 5 + ((ud - 1 * numb) * 10)) <> RGB(255, 0, 0) Then
                                                    ud = ud - 1 * numb
                                                    pathy(shortestchoosey, movey) = -1 * numb
                                                    If pathud <> -2 Then
                                                        pathud = -1
                                                    End If
                                                ElseIf Point(cleft + 5 + (rL * 10), ctop + 5 + ((ud - 1 * numb) * 10)) = RGB(255, 0, 0) Then
                                                    If Point(cleft + 5 + ((rL - pathx(choosev, movex)) * 10), ctop + 5 + ((ud - pathy(choosev1, movey) - 1) * 10)) <> RGB(255, 0, 0) Then
                                                        rL = rL - pathx(choosev, movex)
                                                        pathx(shortestchoose, movex) = 0
                                                        ud = ud - pathy(choosev1, movey)
                                                        pathy(shortestchoosey, movey) = 0
                                                        ud = ud - 1 * numb
                                                        pathy(shortestchoosey, movey) = -1 * numb
                                                        pathud = -2
                                                    ElseIf Point(cleft + 5 + ((rL - pathx(choosev, movex)) * 10), ctop + 5 + ((ud - pathy(choosev1, movey) - 1) * 10)) = RGB(255, 0, 0) Then
                                                        If Point(cleft + 5 + (rL * 10), ctop + 5 + ((ud + 1 * numb) * 10)) <> RGB(255, 0, 0) Then
                                                            ud = ud + 1 * numb
                                                            pathy(shortestchoosey, movey) = 1 * numb
                                                            If pathud <> 2 Then
                                                            pathud = 1
                                                            End If
                                                        ElseIf Point(cleft + 5 + (rL * 10), ctop + 5 + ((ud + 1 * numb) * 10)) = RGB(255, 0, 0) Then
                                                            If Point(cleft + 5 + ((rL - pathx(choosev, movex)) * 10), ctop + 5 + ((ud - pathy(choosev1, movey) + 1) * 10)) <> RGB(255, 0, 0) Then
                                                                rL = rL - pathx(choosev, movex)
                                                                pathx(shortestchoose, movex) = 0
                                                                ud = ud - pathy(choosev1, movey)
                                                                pathy(shortestchoosey, movey) = 0
                                                                ud = ud + 1 * numb
                                                                pathy(shortestchoosey, movey) = 1 * numb
                                                                pathud = 2
                                                            End If
                                                        End If
                                                    End If
                                                End If
                                            ElseIf Point(cleft + 5, ctop + 5 + (ud * 10)) = RGB(255, 0, 0) Then
                                                If Point(cleft + 5 + ((rL - 1 * numb) * 10), ctop + 5 + (ud * 10)) <> RGB(255, 0, 0) Then
                                                    rL = rL - 1 * numb
                                                    pathx(shortestchoose, movex) = -1 * numb
                                                    If pathrl <> -2 Then
                                                            pathrl = -1
                                                            End If
                                                ElseIf Point(cleft + 5 + ((rL - 1 * numb) * 10), ctop + 5 + (ud * 10)) = RGB(255, 0, 0) Then
                                                    If Point(cleft + 5 + ((rL - pathx(choosev, movex) - 1) * 10), ctop + 5 + ((ud - pathy(choosev1, movey)) * 10)) <> RGB(255, 0, 0) Then
                                                        rL = rL - pathx(choosev, movex)
                                                        pathx(shortestchoose, movex) = 0
                                                        ud = ud - pathy(choosev1, movey)
                                                        pathy(shortestchoosey, movey) = 0
                                                        rL = rL - 1 * numb
                                                        pathx(shortestchoose, movex) = -1 * numb
                                                        pathrl = -2
                                                    ElseIf Point(cleft + 5 + ((rL - pathx(choosev, movex) - 1) * 10), ctop + 5 + ((ud - pathy(choosev1, movey)) * 10)) = RGB(255, 0, 0) Then
                                                        If Point(cleft + 5 + ((rL + 1 * numb) * 10), ctop + 5 + (ud * 10)) <> RGB(255, 0, 0) Then
                                                            rL = rL + 1 * numb
                                                            pathx(shortestchoose, movex) = 1 * numb
                                                            If pathrl <> 2 Then
                                                                pathrl = 1
                                                            End If
                                                        ElseIf Point(cleft + 5 + ((rL + 1 * numb) * 10), ctop + 5 + (ud * 10)) = RGB(255, 0, 0) Then
                                                            If Point(cleft + 5 + ((rL - pathx(choosev, movex) + 1) * 10), ctop + 5 + ((ud - pathy(choosev1, movey)) * 10)) <> RGB(255, 0, 0) Then
                                                                rL = rL - pathx(choosev, movex)
                                                                pathx(shortestchoose, movex) = 0
                                                                ud = ud - pathy(choosev1, movey)
                                                                pathy(shortestchoosey, movey) = 0
                                                                rL = rL + 1 * numb
                                                                pathx(shortestchoose, movex) = 1 * numb
                                                                pathrl = 2
                                                            End If
                                                        End If
                                                    End If
                                                End If
                                            End If
                                        
                                        End If
                                        If flag$(2) <> "yes" Then
                                        If pathrl = 2 Then
                                            pathru = pathru + 1
                                            If pathru >= 3 And Point(cleft + (rL * 10) + 5 - 10, ctop + 5 + (ud * 10) + 5) <> RGB(255, 0, 0) Then
                                                rL = rL - 1
                                                pathrl = 1
                                                pathx(shortestchoose, movex) = -1
                                            End If
                                        ElseIf pathrl = 1 Then
                                            pathru = pathru + 1
                                            If pathru >= 2 And Point(cleft + (rL * 10) + 5 - 10, ctop + 5 + (ud * 10) + 5) <> RGB(255, 0, 0) Then
                                                rL = rL - 1
                                                pathx(shortestchoose, movex) = -1
                                                pathrl = 0
                                                pathru = 0
                                            End If
                                        End If
                                        If pathrl = -2 Then
                                            pathru = pathru + 1
                                            If pathru >= 3 And Point(cleft + (rL * 10) + 5 + 10, ctop + 5 + (ud * 10) + 5) <> RGB(255, 0, 0) Then
                                                rL = rL + 1
                                                pathx(shortestchoose, movex) = 1
                                                pathrl = -1
                                            End If
                                        ElseIf pathrl = -1 Then
                                            pathru = pathru + 1
                                            If pathru >= 2 And Point(cleft + (rL * 10) + 5 + 10, ctop + 5 + (ud * 10) + 5) <> RGB(255, 0, 0) Then
                                                rL = rL + 1
                                                pathx(shortestchoose, movex) = 1
                                                pathrl = 0
                                                pathru = 0
                                            End If
                                        End If
                                        If pathud = 2 Then
                                            pathrud = pathrud + 1
                                            If pathrud >= 3 And Point(cleft + (rL * 10) + 5, ctop + 5 + (ud * 10) + 5 - 10) <> RGB(255, 0, 0) Then
                                                ud = ud - 1
                                                pathy(shortestchoosey, movey) = -1
                                                pathud = 1
                                            End If
                                        ElseIf pathud = 1 Then
                                            pathrud = pathrud + 1
                                            If pathrud >= 2 And Point(cleft + (rL * 10) + 5, ctop + 5 + (ud * 10) + 5 - 10) <> RGB(255, 0, 0) Then
                                                ud = ud - 1
                                                pathud = 0
                                                 pathy(shortestchoosey, movey) = -1
                                                pathrud = 0
                                            End If
                                        End If
                                        If pathud = -2 Then
                                            pathrud = pathrud + 1
                                            If pathrud >= 3 And Point(cleft + (rL * 10) + 5, ctop + 5 + (ud * 10) + 5 + 10) <> RGB(255, 0, 0) Then
                                                ud = ud + 1
                                                pathud = -1
                                                 pathy(shortestchoosey, movey) = 1
                                            End If
                                        ElseIf pathud = -1 Then
                                            pathrud = pathrud + 1
                                            If pathrud >= 2 And Point(cleft + (rL * 10) + 5, ctop + 5 + (ud * 10) + 5 + 10) <> RGB(255, 0, 0) Then
                                                ud = ud + 1
                                                pathud = 0
                                                pathrud = 0
                                                 pathy(shortestchoosey, movey) = 1
                                            End If
                                        End If
                                        End If
                        movex = movex + 1
                        movey = movey + 1
                        cleft = cleft + (rL * 10)
                        ctop = ctop + (ud * 10)
                    Loop Until Point(cleft + 5, ctop + 5) = RGB(0, vert, bleu) Or dontdothis = True
                    End If
                    If methodavoid = 1 Then
                        Do
                        rL = rL + pathx(shortestchoose, movex)
                        ud = ud + pathy(shortestchoosey, movey)
                        movex = movex + 1
                        movey = movey + 1
                        cleft = cleft + (rL * 10)
                        ctop = ctop + (ud * 10)
                        Loop Until Point(cleft + 5, ctop + 5) = RGB(0, vert, bleu)
                    End If
                    If flag$(2) <> "yes" Then
                        Do
                                If rL > 0 Then
                                    rL = rL - 1
                                    pathx(shortestchoose, movex) = -1
                                ElseIf rL < 0 Then
                                    pathx(shortestchoose, movex) = 1
                                    rL = rL + 1
                                End If
                                If ud > 0 Then
                                    ud = ud - 1
                                    pathy(shortestchoosey, movey) = -1
                                ElseIf ud < 0 Then
                                    pathy(shortestchoosey, movey) = 1
                                    ud = ud + 1
                                End If
                                ctop = ctop + (ud * 10)
                                movey = movey + 1
                                cleft = cleft + (rL * 10)
                                movex = movex + 1
                            Loop Until rL = 0 And ud = 0
                    End If
                    End If
                    End If
                    If shortestchoose = 0 And short = 0 And shortestchoosey = 0 Then
                        ai2(blgr) = True
                        ai1(blgr) = False
                    End If
                    End If
                    If ai2(blgr) = True Then
                    choosev = 1
                        avoidshort = 0
                        For fun2 = 1 To 1000
                For fun = 1 To 1000
                    pathx(fun, fun2) = 0
                    pathy(fun, fun2) = 0
                Next
                Next
                        Do
                        avoidshort = avoidshort + 1
                        numberavoid = 0
                        Do
                        numberavoid = numberavoid + 1
                        checkavoid = 0
                        checkpath = 0
                        Do
                        checkavoid = checkavoid + 1
                        trynumberavoid = 0
                        fakecleft = 0
                        fakectop = 0
                        fakerl = 0
                        fakeud = 0
                        fakemovex = 0
                        fakemovey = 0
                        rL = 0
                        ud = 0
                        cleft = Command(2).Left
                        ctop = Command(2).Top
                        movex = 1
                        movey = 1
                        dontdoavoid = False
                        choosev = choosev + 1
                        path = 0
                        dontdothis = False
                        Do
                        If Point(cleft + 5, yy + 5) = RGB(0, 0, 255) Then
                            If Point(cleft + 5, ctop + 15) = RGB(255, 0, 0) And ctop < yy Then
                                If Point(cleft - 5, ctop + 5) <> RGB(255, 0, 0) And Point(cleft - 5, yy + 5) = RGB(0, 0, 255) Then
                                    fakecleft = cleft
                                    dontdoavoid = False
                                    Do
                                        If Point(fakecleft - 5, ctop + 5) <> RGB(255, 0, 0) And Point(fakecleft - 5, yy + 5) = RGB(0, 0, 255) Then
                                            fakecleft = fakecleft - 10
                                        Else
                                            dontdoavoid = True
                                        End If
                                    Loop Until Point(fakecleft + 5, ctop + 15) <> RGB(255, 0, 0) Or dontdoavoid = True
                                    If dontdoavoid = False Then
                                        movex = movex - 1
                                        movey = movey - 1
                                        Do
                                            movex = movex + 1
                                            cleft = cleft - 10
                                            pathx(choosev, movex) = -1
                                            movey = movey + 1
                                            pathy(choosev, movey) = 0
                                            
                                        Loop Until Point(cleft + 5, ctop + 15) <> RGB(255, 0, 0)
                                    End If
                                ElseIf Point(cleft + 15, ctop + 5) <> RGB(255, 0, 0) And Point(cleft + 15, yy + 5) = RGB(0, 0, 255) Then
                                    fakecleft = cleft
                                    dontdoavoid = False
                                    Do
                                        If Point(fakecleft + 15, ctop + 5) <> RGB(255, 0, 0) And Point(fakecleft + 15, yy + 5) = RGB(0, 0, 255) Then
                                            fakecleft = fakecleft + 10
                                        Else
                                            dontdoavoid = True
                                        End If
                                    Loop Until Point(fakecleft + 5, ctop + 15) <> RGB(255, 0, 0) Or dontdoavoid = True
                                    If dontdoavoid = False Then
                                        movex = movex - 1
                                        movey = movey - 1
                                        Do
                                            movey = movey + 1
                                            pathy(choosev, movey) = 0
                                            movex = movex + 1
                                            cleft = cleft + 10
                                            pathx(choosev, movex) = 1
                                            
                                        Loop Until Point(cleft + 5, ctop + 15) <> RGB(255, 0, 0)
                                    End If
                                End If
                            ElseIf Point(cleft + 5, ctop - 5) = RGB(255, 0, 0) And ctop > yy Then
                                If Point(cleft - 5, ctop + 5) <> RGB(255, 0, 0) And Point(cleft - 5, yy + 5) = RGB(0, 0, 255) And right2 = False Then
                                    fakecleft = cleft
                                    dontdoavoid = False
                                    Do
                                        If Point(fakecleft - 5, ctop + 5) <> RGB(255, 0, 0) And Point(fakecleft - 5, yy + 5) = RGB(0, 0, 255) Then
                                            fakecleft = fakecleft - 10
                                        Else
                                            dontdoavoid = True
                                        End If
                                    Loop Until Point(fakecleft + 5, ctop - 5) <> RGB(255, 0, 0) Or dontdoavoid = True
                                    If dontdoavoid = False Then
                                        movex = movex - 1
                                        movey = movey - 1
                                        Do
                                            movey = movey + 1
                                            pathy(choosev, movey) = 0
                                            movex = movex + 1
                                            cleft = cleft - 10
                                            pathx(choosev, movex) = -1
                                        Loop Until Point(cleft + 5, ctop - 5) <> RGB(255, 0, 0)
                                    End If
                                ElseIf Point(cleft + 15, ctop + 5) <> RGB(255, 0, 0) And Point(cleft + 15, yy + 5) = RGB(0, 0, 255) Then
                                    fakecleft = cleft
                                    dontdoavoid = False
                                    Do
                                        If Point(fakecleft + 15, ctop + 5) <> RGB(255, 0, 0) And Point(fakecleft + 15, yy + 5) = RGB(0, 0, 255) Then
                                            fakecleft = fakecleft + 10
                                        Else
                                            dontdoavoid = True
                                        End If
                                    Loop Until Point(fakecleft + 5, ctop - 5) <> RGB(255, 0, 0) Or dontdoavoid = True
                                    If dontdoavoid = False Then
                                        movex = movex - 1
                                        movey = movey - 1
                                        Do
                                            movey = movey + 1
                                            pathy(choosev, movey) = 0
                                            movex = movex + 1
                                            cleft = cleft + 10
                                            pathx(choosev, movex) = 1
                                        Loop Until Point(cleft + 5, ctop - 5) <> RGB(255, 0, 0)
                                    End If
                                End If
                            End If
                        ElseIf Point(xx + 5, ctop + 5) = RGB(0, 0, 255) Then
                            If Point(cleft + 15, ctop + 5) = RGB(255, 0, 0) And cleft < xx Then
                                If Point(cleft + 5, ctop - 5) <> RGB(255, 0, 0) And Point(xx + 5, ctop - 5) = RGB(0, 0, 255) Then
                                    fakectop = ctop
                                    dontdoavoid = False
                                    Do
                                        If Point(cleft + 5, fakectop - 5) <> RGB(255, 0, 0) And Point(xx + 5, fakectop - 5) = RGB(0, 0, 255) Then
                                            fakectop = fakectop - 10
                                        Else
                                            dontdoavoid = True
                                        End If
                                    Loop Until Point(cleft + 15, fakectop + 5) <> RGB(255, 0, 0) Or dontdoavoid = True
                                    If dontdoavoid = False Then
                                        movey = movey - 1
                                        movex = movex - 1
                                        Do
                                            movex = movex + 1
                                            pathx(choosev, movex) = 0
                                            movey = movey + 1
                                            ctop = ctop - 10
                                            pathy(choosev, movey) = -1
                                        Loop Until Point(cleft + 15, ctop + 5) <> RGB(255, 0, 0)
                                    End If
                                ElseIf Point(cleft + 5, ctop + 15) <> RGB(255, 0, 0) And Point(xx + 5, ctop + 15) = RGB(0, 0, 255) Then
                                    fakectop = ctop
                                    dontdoavoid = False
                                    Do
                                        If Point(cleft + 5, fakectop + 15) <> RGB(255, 0, 0) And Point(xx + 5, fakectop + 15) = RGB(0, 0, 255) Then
                                            fakectop = fakectop - 10
                                        Else
                                            dontdoavoid = True
                                        End If
                                    Loop Until Point(cleft + 15, fakectop + 5) <> RGB(255, 0, 0) Or dontdoavoid = True
                                    If dontdoavoid = False Then
                                        movey = movey - 1
                                        movex = movex - 1
                                        Do
                                            movex = movex + 1
                                            pathx(choosev, movex) = 0
                                            movey = movey + 1
                                            ctop = ctop - 10
                                            pathy(choosev, movey) = -1
                                        Loop Until Point(cleft + 15, ctop + 5) <> RGB(255, 0, 0)
                                    End If
                                End If
                            ElseIf Point(cleft - 5, ctop + 5) = RGB(255, 0, 0) And cleft > xx Then
                                If Point(cleft + 5, ctop - 5) <> RGB(255, 0, 0) And Point(xx + 5, ctop - 5) = RGB(0, 0, 255) And down2 = False Then
                                    fakectop = ctop
                                    dontdoavoid = False
                                    Do
                                        If Point(cleft + 5, fakectop - 5) <> RGB(255, 0, 0) And Point(xx + 5, fakectop - 5) = RGB(0, 0, 255) Then
                                            fakectop = fakectop - 10
                                        Else
                                            dontdoavoid = True
                                        End If
                                    Loop Until Point(cleft - 5, fakectop + 5) <> RGB(255, 0, 0) Or dontdoavoid = True
                                    If dontdoavoid = False Then
                                        movey = movey - 1
                                        movex = movex - 1
                                        Do
                                            movex = movex + 1
                                            pathx(choosev, movex) = 0
                                            movey = movey + 1
                                            ctop = ctop - 10
                                            pathy(choosev, movey) = -1
                                        Loop Until Point(cleft - 5, ctop + 5) <> RGB(255, 0, 0)
                                    End If
                                ElseIf Point(cleft + 5, ctop + 15) <> RGB(255, 0, 0) And Point(xx + 5, ctop + 15) = RGB(0, 0, 255) Then
                                    fakectop = ctop
                                    dontdoavoid = False
                                    Do
                                        If Point(cleft + 5, fakectop + 15) <> RGB(255, 0, 0) And Point(xx + 5, fakectop + 15) = RGB(0, 0, 255) Then
                                            fakectop = fakectop - 10
                                        Else
                                            dontdoavoid = True
                                        End If
                                    Loop Until Point(cleft - 5, fakectop + 5) <> RGB(255, 0, 0) Or dontdoavoid = True
                                    If dontdoavoid = False Then
                                        movey = movey - 1
                                        movex = movex - 1
                                        Do
                                            movex = movex + 1
                                            pathx(choosev, movex) = 0
                                            movey = movey + 1
                                            ctop = ctop - 10
                                            pathy(choosev, movey) = -1
                                        Loop Until Point(cleft - 5, ctop + 5) <> RGB(255, 0, 0)
                                    End If

                                End If
                            End If
                        End If
                        If cleft > xx And Point(cleft + 5, yy + 5) <> RGB(0, 0, 255) Or avoidleft(1) = True Or avoidleft(2) = True Then
                If right1 = False And avoidright(1) = False And avoidright(2) = False And avoidtop(1) = False And avoidtop(2) = False And avoiddown(1) = False And avoiddown(2) = False Then
                    If Point(cleft - 5, ctop + 5) <> RGB(255, 0, 0) Then
                        rL = -1
                        pathx(choosev, movex) = -1
                        left1 = True
                        If avoidleft(1) = True Or avoidleft(2) = True Then
                            glitch = glitch + 1
                            If glitch = 2 Then
                                If Point(cleft + 15, ctop - 5) <> RGB(255, 0, 0) Then
                                    avoidleft(1) = False
                                    glitch = 0
                                End If
                                If Point(cleft + 15, ctop + 15) <> RGB(255, 0, 0) Then
                                    avoidleft(2) = False
                                    glitch = 0
                                End If
                            End If
                        End If
                    ElseIf Point(cleft - 5, ctop + 5) = RGB(255, 0, 0) Then
                        If Point(cleft + 5, ctop + 15) <> RGB(255, 0, 0) And ctop + 15 > yy And Point(cleft + 5, ctop - 5) = RGB(255, 0, 0) Or Point(cleft + 5, ctop - 5) <> RGB(255, 0, 0) And ctop - 5 < yy And Point(cleft + 5, ctop + 15) = RGB(255, 0, 0) Or ctop = yy Or Point(xx + 5, ctop + 5) = RGB(0, 0, 255) Or avoidleft(1) = True Or avoidleft(2) = True Then
                            If avoidleft(1) = False And avoidleft(2) = False Then
                                fakemovey = movey
                                dontdoavoid = False
                                fakectop = ctop
                                fakecleft = cleft
                                fakemovex = movex
                                
                                Do
                                    fakemovey = fakemovey - 1
                                    fakectop = fakectop - (pathy(choosev, fakemovey) * 10)
                                    fakemovex = fakemovex - 1
                                    fakecleft = fakecleft - (pathx(choosev, fakemovex) * 10)
                                    If Point(fakecleft + 5, fakectop + 5) = RGB(255, 0, 0) Or fakemovey = 0 Then dontdoavoid = True
                                
                                Loop Until Point(cleft - 5, fakectop + 5) <> RGB(255, 0, 0) Or dontdoavoid = True
                                If dontdoavoid = False Then
                                    Do
                                        fakecleft = fakecleft - 10
                                        If Point(fakecleft + 5, fakectop + 5) = RGB(255, 0, 0) Then dontdoavoid = True
                                    Loop Until fakecleft = cleft Or dontdoavoid = True
                                End If
                                If dontdoavoid = False Then
                                    fakemovey = movey
                                    Do
                                        fakemovey = fakemovey - 1
                                        ctop = ctop - (pathy(choosev, fakemovey) * 10)
                                        pathy(choosev, fakemovey) = 0
                                    Loop Until Point(cleft - 5, ctop + 5) <> RGB(255, 0, 0)
                                If Point(cleft - 5, ctop + 5) <> RGB(255, 0, 0) Then
                                    rL = -1
                                    pathx(choosev, movex) = -1
                                    left1 = True
                                    If ctop < yy Then avoidleft(1) = True
                                    If ctop > yy Then avoidleft(2) = True
                                End If
                                End If
                                
                            End If
                            If rL = 0 Or dontdoavoid = True And avoidshort <> 1 Then
                            dontdoavoid = False
                            If avoidleft(1) = False And avoidleft(2) = False Then
                                
                                fakemovey = movey
                                fakectop = ctop
                                fakecleft = cleft
                                fakemovex = movex
                                Do
                                    If avoidshort = 2 Then fakectop = fakectop + 10
                                    If avoidshort = 3 Then fakectop = fakectop - 10
                                    fakemovex = fakemovex - 1
                                    fakecleft = fakecleft - (pathx(choosev, fakemovex) * 10)
                                    If Point(fakecleft + 5, fakectop + 5) = RGB(255, 0, 0) Or fakemovex = 0 Or fakemovey = 0 Then dontdoavoid = True
                                Loop Until Point(cleft - 5, fakectop + 5) <> RGB(255, 0, 0) Or dontdoavoid = True
                                If dontdoavoid = False Then
                                    fakemovey = movey
                                    Do
                                        fakemovey = fakemovey - 1
                                        If avoidshort = 2 Then
                                            ctop = ctop + 10
                                            pathy(choosev, fakemovey) = 1
                                        ElseIf avoidshort = 3 Then
                                            ctop = ctop - 10
                                            pathy(choosev, fakemovey) = -1
                                        End If
                                    Loop Until Point(cleft - 5, ctop + 5) <> RGB(255, 0, 0)
                                If Point(cleft - 5, ctop + 5) <> RGB(255, 0, 0) Then
                                    rL = -1
                                    pathx(choosev, movex) = -1
                                    left1 = True
                                End If
                                usingavoid1 = True
                                End If
                            End If
                            End If
                            If dontdoavoid = True Or rL = 0 Then
                            trynumberavoid = trynumberavoid + 1
                            If trynumberavoid = numberavoid Then
                                way(avoidshort, numberavoid) = checkavoid
                            End If
                            If Point(cleft + 5, ctop + 15) <> RGB(255, 0, 0) And avoidleft(2) = False And way(avoidshort, numberavoid) = 1 Then
                                ud = 1
                                pathy(choosev, movey) = 1
                                avoidleft(1) = True
                                If Point(cleft + 5, ctop + 25) = RGB(255, 0, 0) Then
                                    avoidleft(1) = False
                                    avoidleft(2) = True
                                End If
                            ElseIf Point(cleft + 5, ctop - 5) <> RGB(255, 0, 0) And avoidleft(1) = False Then
                                ud = -1
                                pathy(choosev, movey) = -1
                                avoidleft(2) = True
                                If Point(cleft + 5, ctop - 15) = RGB(255, 0, 0) Then
                                    avoidleft(1) = True
                                    avoidleft(2) = False
                                End If
                            ElseIf Point(cleft + 15, ctop + 5) <> RGB(255, 0, 0) Then
                                rL = 1
                            End If
                            End If
                        End If
                    End If
                End If
            ElseIf cleft < xx And Point(cleft + 5, yy + 5) <> RGB(0, 0, 255) Or avoidright(1) = True Or avoidright(2) = True Then
                If left1 = False And avoidleft(1) = False And avoidleft(2) = False And avoidtop(1) = False And avoidtop(2) = False And avoiddown(1) = False And avoiddown(2) = False Then
                    If Point(cleft + 15, ctop + 5) <> RGB(255, 0, 0) Then
                        rL = 1
                        pathx(choosev, movex) = 1
                        right1 = True
                        If avoidright(1) = True Or avoidright(2) = True Then
                            glitch = glitch + 1
                            If glitch = 2 Then
                                If Point(cleft - 5, ctop - 5) <> RGB(255, 0, 0) Then
                                    avoidright(1) = False
                                    glitch = 0
                                End If
                                If Point(cleft - 5, ctop + 15) <> RGB(255, 0, 0) Then
                                    avoidright(2) = False
                                    glitch = 0
                                End If
                            End If
                        End If
                    ElseIf Point(cleft + 15, ctop + 5) = RGB(255, 0, 0) Then
                        If Point(cleft + 5, ctop + 15) <> RGB(255, 0, 0) And ctop + 15 > yy And Point(cleft + 5, ctop - 5) = RGB(255, 0, 0) Or Point(cleft + 5, ctop - 5) <> RGB(255, 0, 0) And ctop - 5 < yy And Point(cleft + 5, ctop + 15) = RGB(255, 0, 0) Or ctop = yy Or Point(xx + 5, ctop + 5) = RGB(0, 0, 255) Or avoidright(1) = True Or avoidright(2) = True Then
                            If avoidright(1) = False And avoidright(2) = False Then
                                fakemovey = movey
                                dontdoavoid = False
                                fakectop = ctop
                                fakecleft = cleft
                                fakemovex = movex
                                
                                Do
                                    fakemovey = fakemovey - 1
                                    fakectop = fakectop - (pathy(choosev, fakemovey) * 10)
                                    fakemovex = fakemovex - 1
                                    fakecleft = fakecleft - (pathx(choosev, fakemovex) * 10)
                                    If Point(fakecleft + 5, fakectop + 5) = RGB(255, 0, 0) Or fakemovex = 0 Then dontdoavoid = True
                                Loop Until Point(cleft + 15, fakectop + 5) <> RGB(255, 0, 0) Or dontdoavoid = True
                                If dontdoavoid = False Then
                                    Do
                                        fakecleft = fakecleft + 10
                                        If Point(fakecleft + 5, fakectop + 5) = RGB(255, 0, 0) Then dontdoavoid = True
                                    Loop Until fakecleft = cleft Or dontdoavoid = True
                                End If
                                If dontdoavoid = False Then
                                    fakemovey = movey
                                    Do
                                        fakemovey = fakemovey - 1
                                        ctop = ctop - (pathy(choosev, fakemovey) * 10)
                                        pathy(choosev, fakemovey) = 0
                                    Loop Until Point(cleft + 15, ctop + 5) <> RGB(255, 0, 0)
                                If Point(cleft + 15, ctop + 5) <> RGB(255, 0, 0) Then
                                    rL = 1
                                    pathx(choosev, movex) = 1
                                    right1 = True
                                    If ctop < yy Then avoidright(1) = True
                                    If ctop > yy Then avoidright(2) = True
                                End If
                                End If
                            End If
                            If rL = 0 Or dontdoavoid = True And avoidshort <> 1 Then
                            dontdoavoid = False
                            If avoidright(1) = False And avoidright(2) = False Then
                                
                                fakemovey = movey
                                fakectop = ctop
                                fakecleft = cleft
                                fakemovex = movex
                                Do
                                    If avoidshort = 2 Then fakectop = fakectop + 10
                                    If avoidshort = 3 Then fakectop = fakectop - 10
                                    fakemovex = fakemovex - 1
                                    fakecleft = fakecleft - (pathx(choosev, fakemovex) * 10)
                                    If Point(fakecleft + 5, fakectop + 5) = RGB(255, 0, 0) Or fakemovex = 0 Or fakemovey = 0 Then dontdoavoid = True
                                Loop Until Point(cleft + 15, fakectop + 5) <> RGB(255, 0, 0) Or dontdoavoid = True
                                If dontdoavoid = False Then
                                    fakemovey = movey
                                    Do
                                        fakemovey = fakemovey - 1
                                        If avoidshort = 2 Then
                                            ctop = ctop + 10
                                            pathy(choosev, fakemovey) = 1
                                        ElseIf avoidshort = 3 Then
                                            ctop = ctop - 10
                                            pathy(choosev, fakemovey) = -1
                                        End If
                                    Loop Until Point(cleft + 15, ctop + 5) <> RGB(255, 0, 0)
                                If Point(cleft + 15, ctop + 5) <> RGB(255, 0, 0) Then
                                    rL = 1
                                    pathx(choosev, movex) = 1
                                    right1 = True
                                End If
                                usingavoid1 = True
                                End If
                            End If
                            End If
                            If dontdoavoid = True Or rL = 0 Then
                            trynumberavoid = trynumberavoid + 1
                            If trynumberavoid = numberavoid Then
                                way(avoidshort, numberavoid) = checkavoid
                            End If
                            If Point(cleft + 5, ctop + 15) <> RGB(255, 0, 0) And avoidright(2) = False And way(avoidshort, numberavoid) = 1 Then
                                ud = 1
                                pathy(choosev, movey) = 1
                                avoidright(1) = True
                                If Point(cleft + 5, ctop + 25) = RGB(255, 0, 0) Then
                                    avoidright(1) = False
                                    avoidright(2) = True
                                End If
                            ElseIf Point(cleft + 5, ctop - 5) <> RGB(255, 0, 0) And avoidright(1) = False Then
                                ud = -1
                                pathy(choosev, movey) = -1
                                avoidright(2) = True
                                If Point(cleft + 5, ctop - 15) = RGB(255, 0, 0) Then
                                    avoidright(1) = True
                                    avoidright(2) = False
                                End If
                            ElseIf Point(cleft - 5, ctop + 5) <> RGB(255, 0, 0) Then
                                rL = -1
                               
                            End If
                            End If
                        End If
                    End If
                End If
            End If
            If ctop > yy And Point(xx + 5, ctop + 5) <> RGB(0, 0, 255) Or avoidtop(1) = True Or avoidtop(2) = True Then
                If avoidright(1) = False And avoidright(2) = False And avoidleft(1) = False And avoidleft(2) = False And avoiddown(1) = False And avoiddown(2) = False Then
                    If left1 = True Then leftright = -5
                    If right1 = True Then leftright = 15
                    If left1 = False And right1 = False Then leftright = 5
                    If Point(cleft + leftright, ctop - 5) <> RGB(255, 0, 0) Then
                        ud = -1
                        pathy(choosev, movey) = -1
                        If avoidtop(1) = True Or avoidtop(2) = True Then
                            glitch = glitch + 1
                            If glitch = 2 Then
                                If Point(cleft - 5, ctop + 15) <> RGB(255, 0, 0) Then
                                    avoidtop(1) = False
                                    glitch = 0
                                End If
                                If Point(cleft + 15, ctop + 15) <> RGB(255, 0, 0) Then
                                    avoidtop(2) = False
                                    glitch = 0
                                End If
                            End If
                        End If
                    ElseIf Point(cleft + leftright, ctop - 5) = RGB(255, 0, 0) And right1 = False And left1 = False Then
                        If Point(cleft + 15, ctop + 5) <> RGB(255, 0, 0) And cleft + 15 > xx And Point(cleft - 5, ctop + 5) = RGB(255, 0, 0) Or Point(cleft - 5, ctop + 5) <> RGB(255, 0, 0) And cleft - 5 < xx And Point(cleft + 15, ctop + 5) = RGB(255, 0, 0) Or cleft = xx Or Point(cleft + 5, yy + 5) = RGB(0, 0, 255) Or avoidtop(1) = True Or avoidtop(2) = True Then
                            
                            If avoidtop(1) = False And avoidtop(2) = False Then
                                fakemovey = movey
                                dontdoavoid = False
                                fakectop = ctop
                                fakecleft = cleft
                                fakemovex = movex
                                
                                Do
                                    fakemovey = fakemovey - 1
                                    fakectop = fakectop - (pathy(choosev, fakemovey) * 10)
                                    fakemovex = fakemovex - 1
                                    fakecleft = fakecleft - (pathx(choosev, fakemovex) * 10)
                                    If Point(fakecleft + 5, fakectop + 5) = RGB(255, 0, 0) Or fakemovex = 0 Then dontdoavoid = True
                                
                                Loop Until Point(fakecleft + 5, ctop - 5) <> RGB(255, 0, 0) Or dontdoavoid = True
                                If dontdoavoid = False Then
                                    Do
                                        fakectop = fakectop - 10
                                        If Point(fakecleft + 5, fakectop + 5) = RGB(255, 0, 0) Then dontdoavoid = True
                                        If fakectop > 2000 Or fakectop < 2000 Then dontdoavoid = True
                                    Loop Until fakectop = ctop Or dontdoavoid = True
                                End If
                                If dontdoavoid = False Then
                                    fakemovex = movex
                                    Do
                                        fakemovex = fakemovex - 1
                                        cleft = cleft - (pathx(choosev, fakemovex) * 10)
                                        pathx(choosev, fakemovex) = 0
                                    Loop Until Point(cleft + 5, ctop - 5) <> RGB(255, 0, 0)
                                If Point(cleft + 5, ctop - 5) <> RGB(255, 0, 0) Then
                                    ud = -1
                                    pathy(choosev, movey) = -1
                                    If cleft < xx Then avoidtop(1) = True
                                    If cleft > xx Then avoidtop(2) = True
                                End If
                                End If
                            End If
                            If ud = 0 Or dontdoavoid = True And avoidshort <> 1 Then
                            dontdoavoid = False
                            If avoidtop(1) = False And avoidtop(2) = False Then
                                
                                fakemovey = movey
                                fakectop = ctop
                                fakecleft = cleft
                                fakemovex = movex
                                Do
                                    If avoidshort = 2 Then fakecleft = fakecleft + 10
                                    If avoidshort = 3 Then fakecleft = fakecleft - 10
                                    fakemovey = fakemovey - 1
                                    fakectop = fakectop - (pathy(choosev, fakemovey) * 10)
                                    If Point(fakecleft + 5, fakectop + 5) = RGB(255, 0, 0) Or fakemovex = 0 Or fakemovey = 0 Then dontdoavoid = True
                                Loop Until Point(fakecleft + 5, ctop - 5) <> RGB(255, 0, 0) Or dontdoavoid = True
                                If dontdoavoid = False Then
                                    fakemovex = movex
                                    Do
                                        fakemovex = fakemovex - 1
                                        If avoidshort = 2 Then
                                            cleft = cleft + 10
                                            pathx(choosev, fakemovex) = 1
                                        ElseIf avoidshort = 3 Then
                                            cleft = cleft - 10
                                            pathx(choosev, fakemovex) = -1
                                        End If
                                    Loop Until Point(cleft + 5, ctop - 5) <> RGB(255, 0, 0)
                                If Point(cleft + 5, ctop - 5) <> RGB(255, 0, 0) Then
                                    ud = -1
                                    pathy(choosev, movey) = -1
                                End If
                                usingavoid1 = True
                                End If
                            End If
                            End If
                            If ud = 0 Or dontdoavoid = True Then
                            trynumberavoid = trynumberavoid + 1
                            If trynumberavoid = numberavoid Then
                                way(avoidshort, numberavoid) = checkavoid
                            End If
                            If Point(cleft + 15, ctop + 5) <> RGB(255, 0, 0) And avoidtop(2) = False And way(avoidshort, numberavoid) = 1 Then
                                rL = 1
                                pathx(choosev, movex) = 1
                                avoidtop(1) = True
                                If Point(cleft + 25, ctop + 5) = RGB(255, 0, 0) Then
                                    avoidtop(1) = False
                                    avoidtop(2) = True
                                End If
                            ElseIf Point(cleft - 5, ctop + 5) <> RGB(255, 0, 0) And avoidtop(1) = False Then
                                rL = -1
                                pathx(choosev, movex) = -1
                                avoidtop(2) = True
                                If Point(cleft - 15, ctop + 5) = RGB(255, 0, 0) Then
                                    avoidtop(1) = True
                                    avoidtop(2) = False
                                End If
                            ElseIf Point(cleft + 5, ctop + 15) <> RGB(255, 0, 0) Then
                                ud = 1
                                
                            End If
                            End If
                        End If
                    End If
                End If
            ElseIf ctop < yy And Point(xx + 5, ctop + 5) <> RGB(0, 0, 255) Or avoiddown(1) = True Or avoiddown(2) = True Then
                If avoidright(1) = False And avoidright(2) = False And avoidtop(1) = False And avoidtop(2) = False And avoidleft(1) = False And avoidleft(2) = False Then
                    If left1 = True Then leftright = -5
                    If right1 = True Then leftright = 15
                    If left1 = False And right1 = False Then leftright = 5
                    If Point(cleft + leftright, ctop + 15) <> RGB(255, 0, 0) Then
                        ud = 1
                        pathy(choosev, movey) = 1
                        If avoiddown(1) = True Or avoiddown(2) = True Then
                            glitch = glitch + 1
                            If glitch = 2 Then
                                If Point(cleft - 5, ctop - 5) <> RGB(255, 0, 0) Then
                                    avoiddown(1) = False
                                    glitch = 0
                                End If
                                If Point(cleft + 15, ctop - 5) <> RGB(255, 0, 0) Then
                                    avoiddown(2) = False
                                    glitch = 0
                                End If
                            End If
                        End If
                    ElseIf Point(cleft + leftright, ctop + 15) = RGB(255, 0, 0) And right1 = False And left1 = False Then
                        If Point(cleft + 15, ctop + 5) <> RGB(255, 0, 0) And cleft + 15 > xx And Point(cleft - 5, ctop + 5) = RGB(255, 0, 0) Or Point(cleft - 5, ctop + 5) <> RGB(255, 0, 0) And cleft - 5 < xx And Point(cleft + 15, ctop + 5) = RGB(255, 0, 0) Or cleft = xx Or Point(cleft + 5, yy + 5) = RGB(0, 0, 255) Or avoiddown(1) = True Or avoiddown(2) = True Then
                            If avoiddown(1) = False And avoiddown(2) = False Then
                                fakemovey = movey
                                dontdoavoid = False
                                fakectop = ctop
                                fakecleft = cleft
                                fakemovex = movex
                                
                                Do
                                    fakemovey = fakemovey - 1
                                    fakectop = fakectop - (pathy(choosev, fakemovey) * 10)
                                    fakemovex = fakemovex - 1
                                    fakecleft = fakecleft - (pathx(choosev, fakemovex) * 10)
                                    If Point(fakecleft + 5, fakectop + 5) = RGB(255, 0, 0) Or fakemovex = 0 Then dontdoavoid = True
                                
                                Loop Until Point(fakecleft + 5, ctop + 15) <> RGB(255, 0, 0) Or dontdoavoid = True
                                If dontdoavoid = False Then
                                    Do
                                        fakectop = fakectop + 10
                                        If Point(fakecleft + 5, fakectop + 5) = RGB(255, 0, 0) Then dontdoavoid = True
                                    Loop Until fakectop = ctop Or dontdoavoid = True
                                End If
                                If dontdoavoid = False Then
                                    fakemovex = movex
                                    Do
                                        fakemovex = fakemovex - 1
                                        cleft = cleft - (pathx(choosev, fakemovex) * 10)
                                        pathx(choosev, fakemovex) = 0
                                    Loop Until Point(cleft + 5, ctop + 15) <> RGB(255, 0, 0)
                                If Point(cleft + 5, ctop + 15) <> RGB(255, 0, 0) Then
                                    ud = 1
                                    pathy(choosev, movey) = 1
                                    If cleft < xx Then avoiddown(1) = True
                                    If cleft > xx Then avoiddown(2) = True
                                End If
                                End If
                            End If
                            If ud = 0 Or dontdoavoid = True And avoidshort <> 1 Then
                            dontdoavoid = False
                            If avoiddown(1) = False And avoiddown(2) = False Then
                                
                                fakemovey = movey
                                fakectop = ctop
                                fakecleft = cleft
                                fakemovex = movex
                                Do
                                    If avoidshort = 2 Then fakeleft = fakeleft + 10
                                    If avoidshort = 3 Then fakeleft = fakeleft - 10
                                    fakemovey = fakemovey - 1
                                    fakectop = fakectop - (pathy(choosev, fakemovey) * 10)
                                    If Point(fakecleft + 5, fakectop + 5) = RGB(255, 0, 0) Or fakemovex = 0 Or fakemovey = 0 Then dontdoavoid = True
                                Loop Until Point(fakecleft + 5, ctop + 15) <> RGB(255, 0, 0) Or dontdoavoid = True
                                If dontdoavoid = False Then
                                    fakemovex = movex
                                    Do
                                        fakemovex = fakemovex - 1
                                        If avoidshort = 2 Then
                                            cleft = cleft + 10
                                            pathx(choosev, fakemovex) = 1
                                        ElseIf avoidshort = 3 Then
                                            cleft = cleft - 10
                                            pathx(choosev, fakemovex) = -1
                                        End If
                                    Loop Until Point(cleft + 5, ctop + 15) <> RGB(255, 0, 0)
                                If Point(cleft + 5, ctop + 15) <> RGB(255, 0, 0) Then
                                    ud = 1
                                    pathy(choosev, movey) = 1
                                End If
                                usingavoid1 = True
                                End If
                            End If
                            End If
                            If ud = 0 Or dontdoavoid = True Then
                            trynumberavoid = trynumberavoid + 1
                            If trynumberavoid = numberavoid Then
                                way(avoidshort, numberavoid) = checkavoid
                            End If
                            If Point(cleft + 15, ctop + 5) <> RGB(255, 0, 0) And avoiddown(2) = False And way(avoidshort, numberavoid) = 1 Then
                                rL = 1
                                pathx(choosev, movex) = 1
                                avoiddown(1) = True
                                If Point(cleft + 25, ctop + 5) = RGB(255, 0, 0) Then
                                    avoiddown(1) = False
                                    avoiddown(2) = True
                                End If
                            ElseIf Point(cleft - 5, ctop + 5) <> RGB(255, 0, 0) And avoiddown(1) = False Then
                                rL = -1
                                pathx(choosev, movex) = -1
                                avoiddown(2) = True
                                If Point(cleft - 15, ctop + 5) = RGB(255, 0, 0) Then
                                    avoiddown(1) = True
                                    avoiddown(2) = False
                                End If
                            ElseIf Point(cleft + 5, ctop - 5) <> RGB(255, 0, 0) Then
                               
                                ud = -1
                            End If
                            End If
                        End If
                    End If
                End If
            End If
            cleft = cleft + rL * 10
            ctop = ctop + ud * 10
            Line (Command(2).Left - rightleft(2) * 10 + 2, Command(2).Top - updown(2) * 10 + 2)-Step(5, 5), RGB(255, p * 255 - 255 + 50, oldp * 50 - 50), BF
            path = path + 1
            movex = movex + 1
            movey = movey + 1
            ud = 0
            rL = 0
            left1 = False
            right1 = False
            If movex = 1000 Or movey = 1000 Then dontdothis = True
            If Point(cleft + 5, ctop + 5) = RGB(255, 0, 0) Then dontdothis = True
            If glitch = 100 Then dontdothis = True
            Loop Until Point(cleft + 5, ctop + 5) = RGB(0, 0, 255) Or dontdothis = True
            If dontdothis = False Then
                cleft = cmdleft
                ctop = cmdtop
                movex = 1
                movey = 1
                Do
                    cleft = cleft + (pathx(choosev, movex) * 10)
                    ctop = ctop + (pathy(choosev, movey) * 10)
                    movex = movex + 1
                    movey = movey + 1
                    If movex = 1000 Or movey = 1000 Then dontdothis = True
                    If Point(cleft + 5, ctop + 5) = RGB(255, 0, 0) Then dontdothis = True
                Loop Until dontdothis = True Or Point(cleft + 5, ctop + 5) = RGB(0, 0, 255)
            End If
            If dontdothis = False And checkavoid = 1 Then checkpath = path
            If path < avoidpath Or avoidpath = 0 Then
                If dontdothis = False And usingavoid1 = True Then
                    avoidpath = path
                    usingavoid1 = False
                End If
            End If
            Loop Until checkavoid = 2
                If checkpath <= path Then
                    way(avoidshort, numberavoid) = 1
                ElseIf path < checkpath Then
                        way(avoidshort, numberavoid) = 2
                End If
            checkpath = 0
            Loop Until numberavoid = 9
            If avoidshort = 1 Then
                avoidpathshort = avoidpath
                avoidpath = 0
            End If
            If avoidshort = 2 Then
                avoidpathshort1 = avoidpath
                avoidpath = 0
            End If
            Loop Until avoidshort = 3
            If avoidpath = 0 Then avoidpath = 1000
            If avoidpathshort = 0 Then avoidpathshort = 1000
            If avoidpathshort1 = 0 Then avoidpathshort1 = 1000
            If avoidpathshort < avoidpath And avoidpathshort < avoidpathshort1 Then
                avoidshort = 1
            ElseIf avoidpathshort1 <= avoidpath And avoidpathshort1 < avoidpathshort Then
                avoidshort = 2
            ElseIf avoidpath < avoidpathshort And avoidpath < avoidpathshort1 Then
                avoidshort = 3
            End If
            If avoidpath = 1000 And avoidpathshort = 1000 And avoidpathshort1 = 1000 Then avoidshort = 1
            trynumberavoid = 1
            rL = 0
            short = 0
                        ud = 0
                        cleft = cmdleft
                        ctop = cmdtop
                        movex = 1
                        movey = 1
                        dontdothis = False
                        choosev = choosev + 1
                        path = 0
                        dontdothis = False
                        down2 = False
                        right2 = False
                        Do
                        If Point(cleft + 5, yy + 5) = RGB(0, 0, 255) Then
                            If Point(cleft + 5, ctop + 15) = RGB(255, 0, 0) And ctop < yy Then
                                If Point(cleft - 5, ctop + 5) <> RGB(255, 0, 0) And Point(cleft - 5, yy + 5) = RGB(0, 0, 255) Then
                                    fakecleft = cleft
                                    dontdoavoid = False
                                    Do
                                        If Point(fakecleft - 5, ctop + 5) <> RGB(255, 0, 0) And Point(fakecleft - 5, yy + 5) = RGB(0, 0, 255) Then
                                            fakecleft = fakecleft - 10
                                        Else
                                            dontdoavoid = True
                                        End If
                                    Loop Until Point(fakecleft + 5, ctop + 15) <> RGB(255, 0, 0) Or dontdoavoid = True
                                    If dontdoavoid = False Then
                                        movex = movex - 1
                                        movey = movey - 1
                                        Do
                                            movex = movex + 1
                                            cleft = cleft - 10
                                            pathx(choosev, movex) = -1
                                            movey = movey + 1
                                            pathy(choosev, movey) = 0
                                            
                                        Loop Until Point(cleft + 5, ctop + 15) <> RGB(255, 0, 0)
                                    End If
                                ElseIf Point(cleft + 15, ctop + 5) <> RGB(255, 0, 0) And Point(cleft + 15, yy + 5) = RGB(0, 0, 255) Then
                                    fakecleft = cleft
                                    dontdoavoid = False
                                    Do
                                        If Point(fakecleft + 15, ctop + 5) <> RGB(255, 0, 0) And Point(fakecleft + 15, yy + 5) = RGB(0, 0, 255) Then
                                            fakecleft = fakecleft + 10
                                        Else
                                            dontdoavoid = True
                                        End If
                                    Loop Until Point(fakecleft + 5, ctop + 15) <> RGB(255, 0, 0) Or dontdoavoid = True
                                    If dontdoavoid = False Then
                                        movex = movex - 1
                                        movey = movey - 1
                                        Do
                                            movey = movey + 1
                                            pathy(choosev, movey) = 0
                                            movex = movex + 1
                                            cleft = cleft + 10
                                            pathx(choosev, movex) = 1
                                            
                                        Loop Until Point(cleft + 5, ctop + 15) <> RGB(255, 0, 0)
                                    End If
                                End If
                            ElseIf Point(cleft + 5, ctop - 5) = RGB(255, 0, 0) And ctop > yy Then
                                If Point(cleft - 5, ctop + 5) <> RGB(255, 0, 0) And Point(cleft - 5, yy + 5) = RGB(0, 0, 255) And right2 = False Then
                                    fakecleft = cleft
                                    dontdoavoid = False
                                    Do
                                        If Point(fakecleft - 5, ctop + 5) <> RGB(255, 0, 0) And Point(fakecleft - 5, yy + 5) = RGB(0, 0, 255) Then
                                            fakecleft = fakecleft - 10
                                        Else
                                            dontdoavoid = True
                                        End If
                                    Loop Until Point(fakecleft + 5, ctop - 5) <> RGB(255, 0, 0) Or dontdoavoid = True
                                    If dontdoavoid = False Then
                                        movex = movex - 1
                                        movey = movey - 1
                                        Do
                                            movey = movey + 1
                                            pathy(choosev, movey) = 0
                                            movex = movex + 1
                                            cleft = cleft - 10
                                            pathx(choosev, movex) = -1
                                        Loop Until Point(cleft + 5, ctop - 5) <> RGB(255, 0, 0)
                                    End If
                                ElseIf Point(cleft + 15, ctop + 5) <> RGB(255, 0, 0) And Point(cleft + 15, yy + 5) = RGB(0, 0, 255) Then
                                    fakecleft = cleft
                                    dontdoavoid = False
                                    Do
                                        If Point(fakecleft + 15, ctop + 5) <> RGB(255, 0, 0) And Point(fakecleft + 15, yy + 5) = RGB(0, 0, 255) Then
                                            fakecleft = fakecleft + 10
                                        Else
                                            dontdoavoid = True
                                        End If
                                    Loop Until Point(fakecleft + 5, ctop - 5) <> RGB(255, 0, 0) Or dontdoavoid = True
                                    If dontdoavoid = False Then
                                        movex = movex - 1
                                        movey = movey - 1
                                        Do
                                            movey = movey + 1
                                            pathy(choosev, movey) = 0
                                            movex = movex + 1
                                            cleft = cleft + 10
                                            pathx(choosev, movex) = 1
                                        Loop Until Point(cleft + 5, ctop - 5) <> RGB(255, 0, 0)
                                    End If
                                End If
                            End If
                        ElseIf Point(xx + 5, ctop + 5) = RGB(0, 0, 255) Then
                            If Point(cleft + 15, ctop + 5) = RGB(255, 0, 0) And cleft < xx Then
                                If Point(cleft + 5, ctop - 5) <> RGB(255, 0, 0) And Point(xx + 5, ctop - 5) = RGB(0, 0, 255) Then
                                    fakectop = ctop
                                    dontdoavoid = False
                                    Do
                                        If Point(cleft + 5, fakectop - 5) <> RGB(255, 0, 0) And Point(xx + 5, fakectop - 5) = RGB(0, 0, 255) Then
                                            fakectop = fakectop - 10
                                        Else
                                            dontdoavoid = True
                                        End If
                                    Loop Until Point(cleft + 15, fakectop + 5) <> RGB(255, 0, 0) Or dontdoavoid = True
                                    If dontdoavoid = False Then
                                        movey = movey - 1
                                        movex = movex - 1
                                        Do
                                            movex = movex + 1
                                            pathx(choosev, movex) = 0
                                            movey = movey + 1
                                            ctop = ctop - 10
                                            pathy(choosev, movey) = -1
                                        Loop Until Point(cleft + 15, ctop + 5) <> RGB(255, 0, 0)
                                    End If
                                ElseIf Point(cleft + 5, ctop + 15) <> RGB(255, 0, 0) And Point(xx + 5, ctop + 15) = RGB(0, 0, 255) Then
                                    fakectop = ctop
                                    dontdoavoid = False
                                    Do
                                        If Point(cleft + 5, fakectop + 15) <> RGB(255, 0, 0) And Point(xx + 5, fakectop + 15) = RGB(0, 0, 255) Then
                                            fakectop = fakectop - 10
                                        Else
                                            dontdoavoid = True
                                        End If
                                    Loop Until Point(cleft + 15, fakectop + 5) <> RGB(255, 0, 0) Or dontdoavoid = True
                                    If dontdoavoid = False Then
                                        movey = movey - 1
                                        movex = movex - 1
                                        Do
                                            movex = movex + 1
                                            pathx(choosev, movex) = 0
                                            movey = movey + 1
                                            ctop = ctop - 10
                                            pathy(choosev, movey) = -1
                                        Loop Until Point(cleft + 15, ctop + 5) <> RGB(255, 0, 0)
                                    End If
                                End If
                            ElseIf Point(cleft - 5, ctop + 5) = RGB(255, 0, 0) And cleft > xx Then
                                If Point(cleft + 5, ctop - 5) <> RGB(255, 0, 0) And Point(xx + 5, ctop - 5) = RGB(0, 0, 255) And down2 = False Then
                                    fakectop = ctop
                                    dontdoavoid = False
                                    Do
                                        If Point(cleft + 5, fakectop - 5) <> RGB(255, 0, 0) And Point(xx + 5, fakectop - 5) = RGB(0, 0, 255) Then
                                            fakectop = fakectop - 10
                                        Else
                                            dontdoavoid = True
                                        End If
                                    Loop Until Point(cleft - 5, fakectop + 5) <> RGB(255, 0, 0) Or dontdoavoid = True
                                    If dontdoavoid = False Then
                                        movey = movey - 1
                                        movex = movex - 1
                                        Do
                                            movex = movex + 1
                                            pathx(choosev, movex) = 0
                                            movey = movey + 1
                                            ctop = ctop - 10
                                            pathy(choosev, movey) = -1
                                        Loop Until Point(cleft - 5, ctop + 5) <> RGB(255, 0, 0)
                                    End If
                                ElseIf Point(cleft + 5, ctop + 15) <> RGB(255, 0, 0) And Point(xx + 5, ctop + 15) = RGB(0, 0, 255) Then
                                    fakectop = ctop
                                    dontdoavoid = False
                                    Do
                                        If Point(cleft + 5, fakectop + 15) <> RGB(255, 0, 0) And Point(xx + 5, fakectop + 15) = RGB(0, 0, 255) Then
                                            fakectop = fakectop - 10
                                        Else
                                            dontdoavoid = True
                                        End If
                                    Loop Until Point(cleft - 5, fakectop + 5) <> RGB(255, 0, 0) Or dontdoavoid = True
                                    If dontdoavoid = False Then
                                        movey = movey - 1
                                        movex = movex - 1
                                        Do
                                            movex = movex + 1
                                            pathx(choosev, movex) = 0
                                            movey = movey + 1
                                            ctop = ctop - 10
                                            pathy(choosev, movey) = -1
                                        Loop Until Point(cleft - 5, ctop + 5) <> RGB(255, 0, 0)
                                    End If

                                End If
                            End If
                        End If
                        If cleft > xx And Point(cleft + 5, yy + 5) <> RGB(0, 0, 255) Or avoidleft(1) = True Or avoidleft(2) = True Then
                If right1 = False And avoidright(1) = False And avoidright(2) = False And avoidtop(1) = False And avoidtop(2) = False And avoiddown(1) = False And avoiddown(2) = False Then
                    If Point(cleft - 5, ctop + 5) <> RGB(255, 0, 0) Then
                        rL = -1
                        pathx(choosev, movex) = -1
                        left1 = True
                        If avoidleft(1) = True Or avoidleft(2) = True Then
                            glitch = glitch + 1
                            If glitch = 2 Then
                                If Point(cleft + 15, ctop - 5) <> RGB(255, 0, 0) Then
                                    avoidleft(1) = False
                                    glitch = 0
                                End If
                                If Point(cleft + 15, ctop + 15) <> RGB(255, 0, 0) Then
                                    avoidleft(2) = False
                                    glitch = 0
                                End If
                            End If
                        End If
                    ElseIf Point(cleft - 5, ctop + 5) = RGB(255, 0, 0) Then
                        If Point(cleft + 5, ctop + 15) <> RGB(255, 0, 0) And ctop + 15 > yy And Point(cleft + 5, ctop - 5) = RGB(255, 0, 0) Or Point(cleft + 5, ctop - 5) <> RGB(255, 0, 0) And ctop - 5 < yy And Point(cleft + 5, ctop + 15) = RGB(255, 0, 0) Or ctop = yy Or Point(xx + 5, ctop + 5) = RGB(0, 0, 255) Or avoidleft(1) = True Or avoidleft(2) = True Then
                            If avoidleft(1) = False And avoidleft(2) = False Then
                                fakemovey = movey
                                dontdoavoid = False
                                fakectop = ctop
                                fakecleft = cleft
                                fakemovex = movex
                                
                                Do
                                    fakemovey = fakemovey - 1
                                    fakectop = fakectop - (pathy(choosev, fakemovey) * 10)
                                    fakemovex = fakemovex - 1
                                    fakecleft = fakecleft - (pathx(choosev, fakemovex) * 10)
                                    If Point(fakecleft + 5, fakectop + 5) = RGB(255, 0, 0) Or fakemovey = 0 Then dontdoavoid = True
                                Loop Until Point(cleft - 5, fakectop + 5) <> RGB(255, 0, 0) Or dontdoavoid = True
                                If dontdoavoid = False Then
                                    Do
                                        fakecleft = fakecleft - 10
                                        If Point(fakecleft + 5, fakectop + 5) = RGB(255, 0, 0) Then dontdoavoid = True
                                    Loop Until fakecleft = cleft Or dontdoavoid = True
                                End If
                                If dontdoavoid = False Then
                                    fakemovey = movey
                                    Do
                                        fakemovey = fakemovey - 1
                                        ctop = ctop - (pathy(choosev, fakemovey) * 10)
                                        pathy(choosev, fakemovey) = 0
                                    Loop Until Point(cleft - 5, ctop + 5) <> RGB(255, 0, 0)
                                If Point(cleft - 5, ctop + 5) <> RGB(255, 0, 0) Then
                                    rL = -1
                                    pathx(choosev, movex) = -1
                                    If ctop < yy Then avoidleft(1) = True
                                    If ctop > yy Then avoidleft(2) = True
                                    left1 = True
                                End If
                                End If
                            End If
                            If rL = 0 Or dontdoavoid = True And avoidshort <> 1 Then
                            dontdoavoid = False
                            If avoidleft(1) = False And avoidleft(2) = False Then
                                
                                fakemovey = movey
                                fakectop = ctop
                                fakecleft = cleft
                                fakemovex = movex
                                Do
                                    If avoidshort = 2 Then fakectop = fakectop + 10
                                    If avoidshort = 3 Then fakectop = fakectop - 10
                                    fakemovex = fakemovex - 1
                                    fakecleft = fakecleft - (pathx(choosev, fakemovex) * 10)
                                    If Point(fakecleft + 5, fakectop + 5) = RGB(255, 0, 0) Or fakemovex = 0 Or fakemovey = 0 Then dontdoavoid = True
                                Loop Until Point(cleft - 5, fakectop + 5) <> RGB(255, 0, 0) Or dontdoavoid = True
                                If dontdoavoid = False Then
                                    fakemovey = movey
                                    Do
                                        fakemovey = fakemovey - 1
                                        If avoidshort = 2 Then
                                            ctop = ctop + 10
                                            pathy(choosev, fakemovey) = 1
                                        ElseIf avoidshort = 3 Then
                                            ctop = ctop - 10
                                            pathy(choosev, fakemovey) = -1
                                        End If
                                    Loop Until Point(cleft - 5, ctop + 5) <> RGB(255, 0, 0)
                                If Point(cleft - 5, ctop + 5) <> RGB(255, 0, 0) Then
                                    rL = -1
                                    pathx(choosev, movex) = -1
                                    left1 = True
                                End If
                                End If
                            End If
                            End If
                            If dontdoavoid = True Or rL = 0 Then
                            If Point(cleft + 5, ctop + 15) <> RGB(255, 0, 0) And avoidleft(2) = False And way(avoidshort, trynumberavoid) = 1 Then
                                trynumberavoid = trynumberavoid + 1
                                ud = 1
                                pathy(choosev, movey) = 1
                                avoidleft(1) = True
                                If Point(cleft + 5, ctop + 25) = RGB(255, 0, 0) Then
                                    avoidleft(1) = False
                                    avoidleft(2) = True
                                End If
                            ElseIf Point(cleft + 5, ctop - 5) <> RGB(255, 0, 0) And avoidleft(1) = False Then
                                trynumberavoid = trynumberavoid + 1
                                ud = -1
                                pathy(choosev, movey) = -1
                                avoidleft(2) = True
                                If Point(cleft + 5, ctop - 15) = RGB(255, 0, 0) Then
                                    way(avoidshort, trynumberavoid) = 1
                                    trynumberavoid = trynumberavoid - 1
                                    avoidleft(1) = True
                                    avoidleft(2) = False
                                End If
                            ElseIf Point(cleft + 15, ctop + 5) <> RGB(255, 0, 0) Then
                                rL = 1
                            End If
                            End If
                        End If
                    End If
                End If
            ElseIf cleft < xx And Point(cleft + 5, yy + 5) <> RGB(0, 0, 255) Or avoidright(1) = True Or avoidright(2) = True Then
                If left1 = False And avoidleft(1) = False And avoidleft(2) = False And avoidtop(1) = False And avoidtop(2) = False And avoiddown(1) = False And avoiddown(2) = False Then
                    If Point(cleft + 15, ctop + 5) <> RGB(255, 0, 0) Then
                        rL = 1
                        pathx(choosev, movex) = 1
                        right1 = True
                        If avoidright(1) = True Or avoidright(2) = True Then
                            glitch = glitch + 1
                            If glitch = 2 Then
                                If Point(cleft - 5, ctop - 5) <> RGB(255, 0, 0) Then
                                    avoidright(1) = False
                                    glitch = 0
                                End If
                                If Point(cleft - 5, ctop + 15) <> RGB(255, 0, 0) Then
                                    avoidright(2) = False
                                    glitch = 0
                                End If
                            End If
                        End If
                    ElseIf Point(cleft + 15, ctop + 5) = RGB(255, 0, 0) Then
                        If Point(cleft + 5, ctop + 15) <> RGB(255, 0, 0) And ctop + 15 > yy And Point(cleft + 5, ctop - 5) = RGB(255, 0, 0) Or Point(cleft + 5, ctop - 5) <> RGB(255, 0, 0) And ctop - 5 < yy And Point(cleft + 5, ctop + 15) = RGB(255, 0, 0) Or ctop = yy Or Point(xx + 5, ctop + 5) = RGB(0, 0, 255) Or avoidright(1) = True Or avoidright(2) = True Then
                            If avoidright(1) = False And avoidright(2) = False Then
                                fakemovey = movey
                                dontdoavoid = False
                                fakectop = ctop
                                fakecleft = cleft
                                fakemovex = movex
                                
                                Do
                                    fakemovey = fakemovey - 1
                                    fakectop = fakectop - (pathy(choosev, fakemovey) * 10)
                                    fakemovex = fakemovex - 1
                                    fakecleft = fakecleft - (pathx(choosev, fakemovex) * 10)
                                    If Point(fakecleft + 5, fakectop + 5) = RGB(255, 0, 0) Or fakemovey = 0 Then dontdoavoid = True
                                Loop Until Point(cleft + 15, fakectop + 5) <> RGB(255, 0, 0) Or dontdoavoid = True
                                If dontdoavoid = False Then
                                    Do
                                        fakecleft = fakecleft + 10
                                        If Point(fakecleft + 5, fakectop + 5) = RGB(255, 0, 0) Then dontdoavoid = True
                                    Loop Until fakecleft = cleft Or dontdoavoid = True
                                End If
                                If dontdoavoid = False Then
                                    fakemovey = movey
                                    Do
                                        fakemovey = fakemovey - 1
                                        ctop = ctop - (pathy(choosev, fakemovey) * 10)
                                        pathy(choosev, fakemovey) = 0
                                    Loop Until Point(cleft + 15, ctop + 5) <> RGB(255, 0, 0)
                                If Point(cleft + 15, ctop + 5) <> RGB(255, 0, 0) Then
                                    rL = 1
                                    pathx(choosev, movex) = 1
                                    right1 = True
                                    If ctop < yy Then avoidright(1) = True
                                    If ctop > yy Then avoidright(2) = True
                                End If
                                End If
                            End If
                            If rL = 0 Or dontdoavoid = True And avoidshort <> 1 Then
                            dontdoavoid = False
                            If avoidright(1) = False And avoidright(2) = False Then
                                
                                fakemovey = movey
                                fakectop = ctop
                                fakecleft = cleft
                                fakemovex = movex
                                Do
                                    If avoidshort = 2 Then fakectop = fakectop + 10
                                    If avoidshort = 3 Then fakectop = fakectop - 10
                                    fakemovex = fakemovex - 1
                                    fakecleft = fakecleft - (pathx(choosev, fakemovex) * 10)
                                    If Point(fakecleft + 5, fakectop + 5) = RGB(255, 0, 0) Or fakemovex = 0 Or fakemovey = 0 Then dontdoavoid = True
                                Loop Until Point(cleft + 15, fakectop + 5) <> RGB(255, 0, 0) Or dontdoavoid = True
                                If dontdoavoid = False Then
                                    fakemovey = movey
                                    Do
                                        fakemovey = fakemovey - 1
                                        If avoidshort = 2 Then
                                            ctop = ctop + 10
                                            pathy(choosev, fakemovey) = 1
                                        ElseIf avoidshort = 3 Then
                                            ctop = ctop - 10
                                            pathy(choosev, fakemovey) = -1
                                        End If
                                    Loop Until Point(cleft + 15, ctop + 5) <> RGB(255, 0, 0)
                                If Point(cleft + 15, ctop + 5) <> RGB(255, 0, 0) Then
                                    rL = 1
                                    pathx(choosev, movex) = 1
                                    right1 = True
                                End If
                                End If
                            End If
                            End If
                            If dontdoavoid = True Or rL = 0 Then
                            If Point(cleft + 5, ctop + 15) <> RGB(255, 0, 0) And avoidright(2) = False And way(avoidshort, trynumberavoid) = 1 Then
                                trynumberavoid = trynumberavoid + 1
                                ud = 1
                                pathy(choosev, movey) = 1
                                avoidright(1) = True
                                If Point(cleft + 5, ctop + 25) = RGB(255, 0, 0) Then
                                    avoidright(1) = False
                                    avoidright(2) = True
                                End If
                            ElseIf Point(cleft + 5, ctop - 5) <> RGB(255, 0, 0) And avoidright(1) = False Then
                                trynumberavoid = trynumberavoid + 1
                                ud = -1
                                pathy(choosev, movey) = -1
                                avoidright(2) = True
                                If Point(cleft + 5, ctop - 15) = RGB(255, 0, 0) Then
                                    way(avoidshort, trynumberavoid) = 1
                                    trynumberavoid = trynumberavoid - 1
                                    avoidright(1) = True
                                    avoidright(2) = False
                                End If
                            ElseIf Point(cleft - 5, ctop + 5) <> RGB(255, 0, 0) Then
                                rL = -1
                               
                            End If
                            End If
                        End If
                    End If
                End If
            End If
            If ctop > yy And Point(xx + 5, ctop + 5) <> RGB(0, 0, 255) Or avoidtop(1) = True Or avoidtop(2) = True Then
                If avoidright(1) = False And avoidright(2) = False And avoidleft(1) = False And avoidleft(2) = False And avoiddown(1) = False And avoiddown(2) = False Then
                    If left1 = True Then leftright = -5
                    If right1 = True Then leftright = 15
                    If left1 = False And right1 = False Then leftright = 5
                    If Point(cleft + leftright, ctop - 5) <> RGB(255, 0, 0) Then
                        ud = -1
                        pathy(choosev, movey) = -1
                        If avoidtop(1) = True Or avoidtop(2) = True Then
                            glitch = glitch + 1
                            If glitch = 2 Then
                                If Point(cleft - 5, ctop + 15) <> RGB(255, 0, 0) Then
                                    avoidtop(1) = False
                                    glitch = 0
                                End If
                                If Point(cleft + 15, ctop + 15) <> RGB(255, 0, 0) Then
                                    avoidtop(2) = False
                                    glitch = 0
                                End If
                            End If
                        End If
                    ElseIf Point(cleft + leftright, ctop - 5) = RGB(255, 0, 0) And right1 = False And left1 = False Then
                        If Point(cleft + 15, ctop + 5) <> RGB(255, 0, 0) And cleft + 15 > xx And Point(cleft - 5, ctop + 5) = RGB(255, 0, 0) Or Point(cleft - 5, ctop + 5) <> RGB(255, 0, 0) And cleft - 5 < xx And Point(cleft + 15, ctop + 5) = RGB(255, 0, 0) Or cleft = xx Or Point(cleft + 5, yy + 5) = RGB(0, 0, 255) Or avoidtop(1) = True Or avoidtop(2) = True Then
                            If avoidtop(1) = False And avoidtop(2) = False Then
                                fakemovey = movey
                                dontdoavoid = False
                                fakectop = ctop
                                fakecleft = cleft
                                fakemovex = movex
                                
                                Do
                                    fakemovey = fakemovey - 1
                                    fakectop = fakectop - (pathy(choosev, fakemovey) * 10)
                                    fakemovex = fakemovex - 1
                                    fakecleft = fakecleft - (pathx(choosev, fakemovex) * 10)
                                    If Point(fakecleft + 5, fakectop + 5) = RGB(255, 0, 0) Or fakemovex = 0 Then dontdoavoid = True
                                
                                Loop Until Point(fakecleft + 5, ctop - 5) <> RGB(255, 0, 0) Or dontdoavoid = True
                                If dontdoavoid = False Then
                                    Do
                                        fakectop = fakectop - 10
                                        If Point(fakecleft + 5, fakectop + 5) = RGB(255, 0, 0) Then dontdoavoid = True
                                    Loop Until fakectop = ctop Or dontdoavoid = True
                                End If
                                If dontdoavoid = False Then
                                    fakemovex = movex
                                    Do
                                        fakemovex = fakemovex - 1
                                        cleft = cleft - (pathx(choosev, fakemovex) * 10)
                                        pathx(choosev, fakemovex) = 0
                                    Loop Until Point(cleft + 5, ctop - 5) <> RGB(255, 0, 0)
                                If Point(cleft + 5, ctop - 5) <> RGB(255, 0, 0) Then
                                    ud = -1
                                    pathy(choosev, movey) = -1
                                    If cleft < xx Then avoidtop(1) = True
                                    If cleft > xx Then avoidtop(2) = True
                                End If
                                End If
                            End If
                            If ud = 0 Or dontdoavoid = True And avoidshort <> 1 Then
                            dontdoavoid = False
                            If avoidtop(1) = False And avoidtop(2) = False Then
                                
                                fakemovey = movey
                                fakectop = ctop
                                fakecleft = cleft
                                fakemovex = movex
                                Do
                                    If avoidshort = 2 Then fakecleft = fakecleft + 10
                                    If avoidshort = 3 Then fakecleft = fakecleft - 10
                                    fakemovey = fakemovey - 1
                                    fakectop = fakectop - (pathy(choosev, fakemovey) * 10)
                                    If Point(fakecleft + 5, fakectop + 5) = RGB(255, 0, 0) Or fakemovex = 0 Or fakemovey = 0 Then dontdoavoid = True
                                Loop Until Point(fakecleft + 5, ctop - 5) <> RGB(255, 0, 0) Or dontdoavoid = True
                                If dontdoavoid = False Then
                                    fakemovex = movex
                                    Do
                                        fakemovex = fakemovex - 1
                                        If avoidshort = 2 Then
                                            cleft = cleft + 10
                                            pathx(choosev, fakemovex) = 1
                                        ElseIf avoidshort = 3 Then
                                            cleft = cleft - 10
                                            pathx(choosev, fakemovex) = -1
                                        End If
                                    Loop Until Point(cleft + 5, ctop - 5) <> RGB(255, 0, 0)
                                If Point(cleft + 5, ctop - 5) <> RGB(255, 0, 0) Then
                                    ud = -1
                                    pathy(choosev, movey) = -1
                                End If
                                End If
                            End If
                            End If
                            If ud = 0 Or dontdoavoid = True Then
                            If Point(cleft + 15, ctop + 5) <> RGB(255, 0, 0) And avoidtop(2) = False And way(avoidshort, trynumberavoid) = 1 Then
                                trynumberavoid = trynumberavoid + 1
                                rL = 1
                                pathx(choosev, movex) = 1
                                avoidtop(1) = True
                                If Point(cleft + 25, ctop + 5) = RGB(255, 0, 0) Then
                                    avoidtop(1) = False
                                    avoidtop(2) = True
                                End If
                            ElseIf Point(cleft - 5, ctop + 5) <> RGB(255, 0, 0) And avoidtop(1) = False Then
                                trynumberavoid = trynumberavoid + 1
                                rL = -1
                                pathx(choosev, movex) = -1
                                avoidtop(2) = True
                                If Point(cleft - 15, ctop + 5) = RGB(255, 0, 0) Then
                                    way(avoidshort, trynumberavoid) = 1
                                    trynumberavoid = trynumberavoid - 1
                                    avoidtop(1) = True
                                    avoidtop(2) = False
                                End If
                            ElseIf Point(cleft + 5, ctop + 15) <> RGB(255, 0, 0) Then
                                ud = 1
                               
                            End If
                            End If
                        End If
                    End If
                End If
            ElseIf ctop < yy And Point(xx + 5, ctop + 5) <> RGB(0, 0, 255) Or avoiddown(1) = True Or avoiddown(2) = True Then
                If avoidright(1) = False And avoidright(2) = False And avoidtop(1) = False And avoidtop(2) = False And avoidleft(1) = False And avoidleft(2) = False Then
                    If left1 = True Then leftright = -5
                    If right1 = True Then leftright = 15
                    If left1 = False And right1 = False Then leftright = 5
                    If Point(cleft + leftright, ctop + 15) <> RGB(255, 0, 0) Then
                        ud = 1
                        pathy(choosev, movey) = 1
                        If avoiddown(1) = True Or avoiddown(2) = True Then
                            glitch = glitch + 1
                            If glitch = 2 Then
                                If Point(cleft - 5, ctop - 5) <> RGB(255, 0, 0) Then
                                    avoiddown(1) = False
                                    glitch = 0
                                End If
                                If Point(cleft + 15, ctop - 5) <> RGB(255, 0, 0) Then
                                    avoiddown(2) = False
                                    glitch = 0
                                End If
                            End If
                        End If
                    ElseIf Point(cleft + leftright, ctop + 15) = RGB(255, 0, 0) And right1 = False And left1 = False Then
                        If Point(cleft + 15, ctop + 5) <> RGB(255, 0, 0) And cleft + 15 > xx And Point(cleft - 5, ctop + 5) = RGB(255, 0, 0) Or Point(cleft - 5, ctop + 5) <> RGB(255, 0, 0) And cleft - 5 < xx And Point(cleft + 15, ctop + 5) = RGB(255, 0, 0) Or cleft = xx Or Point(cleft + 5, yy + 5) = RGB(0, 0, 255) Or avoiddown(1) = True Or avoiddown(2) = True Then
                            If avoiddown(1) = False And avoiddown(2) = False Then
                                fakemovey = movey
                                dontdoavoid = False
                                fakectop = ctop
                                fakecleft = cleft
                                fakemovex = movex
                                
                                Do
                                    fakemovey = fakemovey - 1
                                    fakectop = fakectop - (pathy(choosev, fakemovey) * 10)
                                    fakemovex = fakemovex - 1
                                    fakecleft = fakecleft - (pathx(choosev, fakemovex) * 10)
                                    If Point(fakecleft + 5, fakectop + 5) = RGB(255, 0, 0) Or fakemovex = 0 Then dontdoavoid = True
                                
                                Loop Until Point(fakecleft + 5, ctop + 15) <> RGB(255, 0, 0) Or dontdoavoid = True
                                If dontdoavoid = False Then
                                    Do
                                        fakectop = fakectop + 10
                                        If Point(fakecleft + 5, fakectop + 5) = RGB(255, 0, 0) Then dontdoavoid = True
                                    Loop Until fakectop = ctop Or dontdoavoid = True
                                End If
                                If dontdoavoid = False Then
                                    fakemovex = movex
                                    Do
                                        fakemovex = fakemovex - 1
                                        cleft = cleft - (pathx(choosev, fakemovex) * 10)
                                        pathx(choosev, fakemovex) = 0
                                    Loop Until Point(cleft + 5, ctop + 15) <> RGB(255, 0, 0)
                                If Point(cleft + 5, ctop + 15) <> RGB(255, 0, 0) Then
                                    ud = 1
                                    pathy(choosev, movey) = 1
                                    If cleft < xx Then avoiddown(1) = True
                                    If cleft > xx Then avoiddown(2) = True
                                End If
                                End If
                            End If
                            If ud = 0 Or dontdoavoid = True And avoidshort <> 1 Then
                            dontdoavoid = False
                            If avoiddown(1) = False And avoiddown(2) = False Then
                                
                                fakemovey = movey
                                fakectop = ctop
                                fakecleft = cleft
                                fakemovex = movex
                                Do
                                    If avoidshort = 2 Then fakecleft = fakecleft + 10
                                    If avoidshort = 3 Then fakecleft = fakecleft - 10
                                    fakemovey = fakemovey - 1
                                    fakectop = fakectop - (pathy(choosev, fakemovey) * 10)
                                    If Point(fakecleft + 5, fakectop + 5) = RGB(255, 0, 0) Or fakemovex = 0 Or fakemovey = 0 Then dontdoavoid = True
                                Loop Until Point(fakecleft + 5, ctop + 15) <> RGB(255, 0, 0) Or dontdoavoid = True
                                If dontdoavoid = False Then
                                    fakemovex = movex
                                    Do
                                        fakemovex = fakemovex - 1
                                        If avoidshort = 2 Then
                                            cleft = cleft + 10
                                            pathx(choosev, fakemovex) = 1
                                        ElseIf avoidshort = 3 Then
                                            cleft = cleft - 10
                                            pathx(choosev, fakemovex) = -1
                                        End If
                                    Loop Until Point(cleft + 5, ctop + 15) <> RGB(255, 0, 0)
                                If Point(cleft + 5, ctop + 15) <> RGB(255, 0, 0) Then
                                    ud = 1
                                    pathy(choosev, movey) = 1
                                End If
                                End If
                            End If
                            End If
                            If ud = 0 Or dontdoavoid = True Then
                            If Point(cleft + 15, ctop + 5) <> RGB(255, 0, 0) And avoiddown(2) = False And way(avoidshort, trynumberavoid) = 1 Then
                                trynumberavoid = trynumberavoid + 1
                                rL = 1
                                pathx(choosev, movex) = 1
                                avoiddown(1) = True
                                If Point(cleft + 25, ctop + 5) = RGB(255, 0, 0) Then
                                    avoiddown(1) = False
                                    avoiddown(2) = True
                                End If
                            ElseIf Point(cleft - 5, ctop + 5) <> RGB(255, 0, 0) And avoiddown(1) = False Then
                                trynumberavoid = trynumberavoid + 1
                                rL = -1
                                pathx(choosev, movex) = -1
                                avoiddown(2) = True
                                If Point(cleft - 15, ctop + 5) = RGB(255, 0, 0) Then
                                    way(avoidshort, trynumberavoid) = 1
                                    trynumberavoid = trynumberavoid - 1
                                    avoiddown(1) = True
                                    avoiddown(2) = False
                                End If
                            ElseIf Point(cleft + 5, ctop - 5) <> RGB(255, 0, 0) Then
                                
                                ud = -1
                            End If
                            End If
                        End If
                    End If
                End If
            End If
            cleft = cleft + rL * 10
            ctop = ctop + ud * 10
            Line (Command(2).Left - rightleft(2) * 10 + 2, Command(2).Top - updown(2) * 10 + 2)-Step(5, 5), RGB(255, p * 255 - 255 + 50, oldp * 50 - 50), BF
            path = path + 1
            movex = movex + 1
            movey = movey + 1
            ud = 0
            rL = 0
            left1 = False
            right1 = False
            If movex = 1000 Or movey = 1000 Then dontdothis = True
            Loop Until Point(cleft + 5, ctop + 5) = RGB(0, 0, 255) Or dontdothis = True
            If dontdothis = False Then
                cleft = cmdleft
                movex = 0
                movey = 0
                ctop = cmdtop
                Do
                    movex = movex + 1
                    cleft = cleft + (pathx(choosev, movex) * 10)
                    movey = movey + 1
                    ctop = ctop + (pathy(choosev, movey) * 10)
                    fakemovex = 0
                    fakecleft = cmdleft
                    fakectop = cmdtop
                    fakemovey = 0
                    Do
                        fakemovex = fakemovex + 1
                        fakecleft = fakecleft + (pathx(choosev, fakemovex) * 10)
                        fakemovey = fakemovey + 1
                        fakectop = fakectop + (pathy(choosev, fakemovey) * 10)
                        If fakecleft = cleft And fakemovex <> movex And pathy(choosev, movey + 1) = 0 Then
                            trycleft = cleft
                            dontdoavoid = False
                            trymovex = movex
                            trymovey = movey
                            Do
                                If trycleft < fakecleft Then trycleft = trycleft + 10
                                trymovey = trymovey + 1
                                trymovex = trymovex + 1
                                If trycleft > fakecleft Then trycleft = trycleft - 10
                                If Point(trycleft + 5, fakectop + 5) = RGB(255, 0, 0) Then dontdoavoid = True
                            Loop Until trycleft = cleft Or dontdoavoid = True
                            If dontdoavoid = False Then
                                tryctop = fakectop
                                Do
                                    trymovex = trymovex + 1
                                    trymovey = trymovey + 1
                                    tryctop = tryctop + (pathy(choosev, trymovey) * 10)
                                    trycleft = trycleft + (pathx(choosev, trymovex) * 10)
                                    If trymovex >= 500 Or trymovey >= 500 Then dontdoavoid = True
                                    If Point(trycleft + 5, tryctop + 5) = RGB(255, 0, 0) Then dontdoavoid = True
                                Loop Until Point(trycleft + 5, tryctop + 5) = RGB(0, 0, 255) Or dontdoavoid = True
                            End If
                            If dontdoavoid = False Then
                                Do
                                    movex = movex + 1
                                    movey = movey + 1
                                    If cleft < fakecleft Then
                                        cleft = cleft + 10
                                        pathx(choosev, movex) = 1
                                    ElseIf cleft > fakecleft Then
                                        cleft = cleft - 10
                                        pathx(choosev, movex) = -1
                                    End If
                                    pathy(choosev, movey) = 0
                                Loop Until cleft = fakecleft
                            End If
                        ElseIf fakectop = ctop And fakemovey <> movey And pathx(choosev, movex + 1) = 0 Then
                            tryctop = ctop
                            dontdoavoid = False
                            trymovex = movex
                            trymovey = movey
                            Do
                                If tryctop < fakectop Then tryctop = tryctop + 10
                                If tryctop > fakectop Then tryctop = tryctop - 10
                                trymovex = trymovex + 1
                                trymovey = trymovey + 1
                                If Point(fakecleft + 5, tryctop + 5) = RGB(255, 0, 0) Then dontdoavoid = True
                            Loop Until tryctop = ctop Or dontdoavoid = True
                            If dontdoavoid = False Then
                                trycleft = fakecleft
                                Do
                                    trymovex = trymovex + 1
                                    trymovey = trymovey + 1
                                    tryctop = tryctop + (pathy(choosev, trymovey) * 10)
                                    trycleft = trycleft + (pathx(choosev, trymovex) * 10)
                                    If trymovex = 1000 Or trymovey = 1000 Then dontdoavoid = True
                                    If Point(trycleft + 5, tryctop + 5) = RGB(255, 0, 0) Then dontdoavoid = True
                                Loop Until Point(trycleft + 5, tryctop + 5) = RGB(0, 0, 255) Or dontdoavoid = True
                            End If
                            If dontdoavoid = False Then
                                Do
                                    movex = movex + 1
                                    movey = movey + 1
                                    If ctop < fakectop Then
                                        ctop = ctop + 10
                                        pathy(choosev, movey) = 1
                                    ElseIf ctop > fakectop Then
                                        ctop = ctop - 10
                                        pathy(choosev, movey) = -1
                                    End If
                                    pathx(choosev, movex) = 0
                                Loop Until ctop = fakectop
                            End If
                        End If
                        
                    Loop Until Point(fakecleft + 5, fakectop + 5) = RGB(0, 0, 255) Or fakemovex = 1000 Or fakemovey = 1000
                Loop Until Point(cleft + 5, ctop + 5) = RGB(0, 0, 255) Or movex = 1000 Or movey = 1000
            End If
                                
                        
              If path < short Or short = 0 Then
                If dontdothis = False Then
                shortestchoose = choosev
                shortestchoosey = choosev
                short = path
                End If
            End If
            
            If dontdothis = False Then
                cleft = cmdleft
                ctop = cmdtop
                movex = 0
                movey = 0
                dontdoavoid = False
                Do
                    movex = movex + 1
                    movey = movey + 1
                    cleft = cleft + pathx(shortestchoose, movex) * 10
                    ctop = ctop + pathy(shortestchoosey, movey) * 10
                    If rlcount > 3 Then
                    If pathx(shortestchoose, movex) <> rlvel Or pathy(shortestchoosey, movey) <> 0 Then
                    
                            choosev = 1
                            short = 0
                            choosev1 = 0
                            Do
                                dontdothis = False
                                choosev = choosev + 1
                                test = 0
                                rL = 0
                                path = -1
                                differance = 0
                                Do
                                    
                                    If test < choosev Then
                                        If rlvel < 0 Then rL = rL - 1
                                        If rlvel > 0 Then rL = rL + 1
                                    ElseIf test >= choosev Then
                                            differance = rlcount - path
                                            If differance >= 6 And differance < 10 Then
                                                If choosev > 3 Then
                                                    If rlvel < 0 Then
                                                        rL = (-3)
                                                    ElseIf rlvel > 0 Then
                                                        rL = 3
                                                    End If
                                                End If
                                            ElseIf differance >= 3 And differance < 6 Then
                                                If choosev > 2 Then
                                                    If rlvel < 0 Then
                                                        rL = (-2)
                                                    ElseIf rlvel > 0 Then
                                                        rL = 2
                                                    End If
                                                End If
                                            ElseIf differance < 3 Then
                                                If choosev > 1 Then
                                                    If rlvel < 0 Then
                                                        rL = (-1)
                                                    ElseIf rlvel > 0 Then
                                                        rL = 1
                                                    End If
                                                End If
                                            End If
                                        End If
                                    test = test + 1
                                    If pathy(shortestchoosey, trymovex + test) <> 0 Then
                                        dontdothis = True
                                    End If
                                    If trymovex > 990 Or trymovey > 990 Or movex > 990 Or movey > 990 Then dontdothis = True
                                    path = path + Abs(rL)
                                    If path = rlcount Then
                                        If rL > 1 Or rL < -1 Then
                                            dontdothis = True
                                        End If
                                    End If
                                    If trymovex >= 900 Then dontdothis = True
                                    If test > rlcount Then dontdothis = True
                                    If path > rlcount Then dontdothis = True
                                Loop Until dontdothis = True Or path = rlcount
                                If choosev > choosev1 Then
                                If test <= short Or short = 0 Or choosev > choosev1 Then
                                    If dontdothis = False Then
                                        short = test
                                        choosev1 = choosev
                                    End If
                                End If
                                End If
                            Loop Until choosev = 4
                            If short <> 0 And choosev1 > 1 Then
                                test = 0
                                rL = 0
                                path = -1
                                Do
                                    trymovex = trymovex + 1
                                    If test < choosev1 Then
                                        If rlvel < 0 Then rL = rL - 1
                                        If rlvel > 0 Then rL = rL + 1
                                        
                                    ElseIf test >= choosev1 Then
                                        differance = rlcount - path
                                            If differance >= 6 And differance < 10 Then
                                                If choosev1 > 3 Then
                                                    If rlvel < 0 Then
                                                        rL = (-3)
                                                    ElseIf rlvel > 0 Then
                                                        rL = 3
                                                    End If
                                                End If
                                            ElseIf differance >= 3 And differance < 6 Then
                                                If choosev1 > 2 Then
                                                    If rlvel < 0 Then
                                                        rL = (-2)
                                                    ElseIf rlvel > 0 Then
                                                        rL = 2
                                                    End If
                                                End If
                                            ElseIf differance < 3 Then
                                                If choosev1 > 1 Then
                                                    If rlvel < 0 Then
                                                        rL = (-1)
                                                    ElseIf rlvel > 0 Then
                                                        rL = 1
                                                    End If
                                                End If
                                            End If
                                    End If
                                    pathx(shortestchoose, trymovex) = rL
                                    test = test + 1
                                    path = path + Abs(rL)
                                Loop Until path = rlcount
                                differance = movex - trymovex
                                For fun = trymovex To 150
                                    pathx(shortestchoose, fun) = pathx(shortestchoose, fun + differance)
                                    pathy(shortestchoosey, fun) = pathy(shortestchoosey, fun + differance)
                                Next
                                movex = trymovex + 1
                                movey = trymovex + 1
                                rlcount = 0
                        End If
                    End If
                    End If
                    If pathy(shortestchoosey, movey) <> udcount Then
                    End If
                    If rlcount = 0 And pathy(shortestchoosey, movey) <> 0 Then
                        rlvel = pathx(shortestchoose, movex)
                        trymovex = movex
                        trycleft = cleft
                        fakemovex = trymovex
                    End If
                    If pathx(shortestchoose, movex) = rlvel And pathy(shortestchoosey, movey) = 0 Then rlcount = rlcount + 1
                    If pathy(shortestchoosey, movey) = udvel Then udcount = udcount + 1
                    
                    If pathx(shortestchoose, movex) <> 0 Or pathy(shortestchoosey, movey) <> udvel Then
                        udvel = pathy(shortestchoosey, movey)
                        tryctop = ctop
                        trymovey = movey
                        udcount = 0
                    End If
                Loop Until pathx(shortestchoose, movex) = 0 And pathy(shortestchoosey, movey) = 0 And pathx(shortestchoose, movex + 1) = 0 And pathy(shortestchoosey, movey + 1) = 0
            End If
            
                    End If
                    If shortestchoose = 0 And shortestchoosey = 0 Then
                        Unload Jump
                        Load Jump
                        Jump.Show vbModal
                    End If
                    If flag$(2) = "no" And shortestchoose <> 0 And shortestchoosey <> 0 Then
                        movex = 1
                        movey = 1
                        cleft = cmdleft
                        ctop = cmdtop
                        rL = 0
                        ud = 0
                        ai2loop = 0
                        Do
                        rL = rL + pathx(shortestchoose, movex)
                        ud = ud + pathy(shortestchoosey, movey)
                        If pathx(shortestchoose, movex) = 0 And pathy(shortestchoosey, movey) = 0 And ai2(1) = True Then
                            If cleft > xx Then rL = -1
                            If cleft < xx Then rL = 1
                            If ctop > yy Then ud = -1
                            If ctop < yy Then ud = 1
                            If cleft > 3000 Or cleft < 3000 Or ctop > 3000 Or ctop < 3000 Then
                                Unload Jump
                                Load Jump
                                Jump.Show vbModal
                            End If
                            pathx(shortestchoose, movex) = rL
                            pathy(shortestchoosey, movey) = ud
                            If ctop + ud * 10 = yy And cleft + rL * 10 = xx Then
                            pathbluex(movex) = pathx(shortestchoose, movex)
                            pathbluey(movey) = pathy(shortestchoosey, movey)
                            movex = movex + 1
                            movey = movey + 1
                            pathx(shortestchoose, movex) = 0
                            pathy(shortestchoosey, movey) = 0
                            End If
                        End If
                        pathbluex(movex) = pathx(shortestchoose, movex)
                        pathbluey(movey) = pathy(shortestchoosey, movey)
                        ctop = ctop + ud * 10
                        cleft = cleft + rL * 10
                        movex = movex + 1
                        movey = movey + 1
                        If ai2(blgr) = True Then
                            rL = 0
                            ud = 0
                            If cleft = xx And ctop = yy Then ai2loop = 1
                        Else
                            If rL = 0 And ud = 0 Then ai2loop = 2
                        End If
                        Loop Until ai2loop = 1 Or ai2loop = 2
                        bluemovex = movex
                        bluemovey = movey
                        cmdleft = cleft
                        cmdtop = ctop
                        flag$(2) = "yes"
                        blgr = 2
                        shortestchooseblue = shortestchoose
                        shortestchoosebluey = shortestchoosey
                        shortestchoose = 0
                shortestchoosey = 0
                    End If
                    If flag$(2) = "yes" And shortestchoose <> 0 And shortestchoosey <> 0 Then
                        shortestchoosegreen = shortestchoose
                        shortestchoosegreeny = shortestchoosey
                    End If
                    movex = 1
                    movey = 1
                blgr = 1
                End If
    Loop Until starter = 2
    LoadAI.Visible = False
    canplay = True
    flag$(2) = "no"
    On Error GoTo start
End Sub
Private Sub Form_KeyDown(KeyCode As Integer, Shift As Integer)
Select Case KeyCode
    Case 27
        End
    Case vbKeyRight
        If p = 1 And canplay = True Then
        If rightleft(p) < rlmax(p) Then
            rightleft(p) = rightleft(p) + 1
            Text1(p) = Abs(rightleft(p))
            Beep 500 * Abs(rightleft(p)), 100
        End If
        If rightleft(p) < 0 Then
            Text4(p).Visible = True
            Text5(p).Visible = False
        ElseIf rightleft(p) > 0 Then
            Text4(p).Visible = False
            Text5(p).Visible = True
        End If
        End If
    Case vbKeyLeft
        If p = 1 And canplay = True Then
        If rightleft(p) > rlmin(p) Then
            rightleft(p) = rightleft(p) - 1
            Text1(p) = Abs(rightleft(p))
            Beep 500 * Abs(rightleft(p)), 100
        End If
        If rightleft(p) < 0 Then
            Text4(p).Visible = True
            Text5(p).Visible = False
        ElseIf rightleft(p) > 0 Then
            Text4(p).Visible = False
            Text5(p).Visible = True
        End If
        End If
    Case vbKeyUp
        If p = 1 And canplay = True Then
        If updown(p) > udmin(p) Then
            updown(p) = updown(p) - 1
            Text(p) = Abs(updown(p))
            Beep 500 * Abs(updown(p)), 100
        End If
        If updown(p) < 0 Then
            Text3(p).Visible = True
            Text2(p).Visible = False
        ElseIf updown(p) > 0 Then
            Text3(p).Visible = False
            Text2(p).Visible = True
        End If
        End If
    Case vbKeyDown
    If p = 1 And canplay = True Then
        If updown(p) < udmax(p) Then
            updown(p) = updown(p) + 1
            Text(p) = Abs(updown(p))
            Beep 500 * Abs(updown(p)), 100
        End If
        If updown(p) < 0 Then
            Text3(p).Visible = True
            Text2(p).Visible = False
        ElseIf updown(p) > 0 Then
            Text3(p).Visible = False
            Text2(p).Visible = True
        End If
        End If
    Case 13
redo:
    If canplay = True Then
        Command(p).Left = Command(p).Left + rightleft(p) * 10
        Command(p).Top = Command(p).Top + updown(p) * 10
        If ai2(blgr) = True Then
            updown(2) = 0
            rightleft(2) = 0
            End If
        Line (Command(p).Left - rightleft(p) * 10 + 2, Command(p).Top - updown(p) * 10 + 2)-Step(5, 5), RGB(255, p * 255 - 255 + 50, oldp * 50 - 50), BF
        oldupdown(p) = updown(p)
        oldrightleft(p) = rightleft(p)
        udmax(p) = updown(p) + 1
        udmin(p) = updown(p) - 1
        rlmax(p) = rightleft(p) + 1
        rlmin(p) = rightleft(p) - 1
        Select Case Point(Command(p).Left + 5, Command(p).Top + 5)
            Case RGB(0, 0, 255)
                flag$(p) = "yes"
                Text6(p).Visible = True
                Text6(p).BackColor = RGB(0, 0, 255)
            Case RGB(0, 255, 0)
                If flag$(p) = "yes" Then
                Text6(p).BackColor = RGB(0, 255, 0)
                score(p) = score(p) + pot
                Text7(p) = score(p)
                StartScreen.Visible = True
                AI.Visible = True
                End If
            Case RGB(255, 0, 0), RGB(0, 0, 0), -1
                For x = 500 To 0 Step -100
                    Beep x, 100
                Next
                Picture1.Visible = True
                Picture1.Top = Command(p).Top - 96
                Picture1.Left = Command(p).Left - 132
                score(p) = score(p) - 10
                score(oldp) = score(oldp) + pot
                    AI.Visible = True
                StartScreen.Visible = True
            Case Else
        End Select
    pot = pot + 1
    oldp = p
    p = p + 1
    If p = 3 Then p = 1
    If p = 2 Then
                        If movex < bluemovex Then
                            rightleft(2) = rightleft(2) + pathbluex(movex)
                        ElseIf movex > bluemovex Then
                            rightleft(2) = rightleft(2) + pathx(shortestchoose, movex)
                        End If
                        If movey < bluemovey Then
                            updown(2) = updown(2) + pathbluey(movey)
                        ElseIf movey > bluemovey Then
                            updown(2) = updown(2) + pathy(shortestchoosey, movey)
                        End If
                    movex = movex + 1
                    movey = movey + 1
                    If movey = bluemovey And movex = bluemovex Then
                        movex = 1
                        movey = 1
                        blgr = 2
                        bluemovey = 0
                        bluemovex = 0
                    End If
                    If rightleft(p) < 0 Then
            Text4(p).Visible = True
            Text5(p).Visible = False
        ElseIf rightleft(p) > 0 Then
            Text4(p).Visible = False
            Text5(p).Visible = True
        End If
        If updown(p) < 0 Then
            Text3(p).Visible = True
            Text2(p).Visible = False
        ElseIf updown(p) > 0 Then
            Text3(p).Visible = False
            Text2(p).Visible = True
        End If
        Text1(p) = Abs(rightleft(p))
            Text(p) = Abs(updown(p))
            GoTo redo
        End If
End If
End Select

End Sub

