Public Class Memo

    Dim P1 As Byte
    Dim P2 As Byte
    Dim P3 As Byte
    Dim P4 As Byte

    Dim NumJugadores As Byte
    Dim TurnoActual As Byte

    Dim clic As Byte
    Dim segundos As Byte

    Dim eleccion_activa As Byte
    Dim eleccion_nueva As Byte

    Dim img_nueva As String
    Dim img_activa As String

    Dim Sc1 As Byte
    Dim Sc2 As Byte
    Dim Sc3 As Byte
    Dim Sc4 As Byte

    Dim max As Integer

    Private Function Aleatorio1(ByVal Minimo As Long, ByRef Maximo As Long) As Long
        Randomize()
        Aleatorio1 = CLng((Minimo - Maximo) * Rnd() + Maximo)
    End Function

    Private Sub Tim_ver_parejas_Tick(sender As Object, e As EventArgs) Handles Tim_ver_parejas.Tick
        segundos = segundos + 1
        Tim_ver_parejas.Enabled = False
        segundos = 0
        'GroupBox1.Enabled = False
        Select Case (eleccion_nueva)
            Case 1 : PictureBox1.Image = Pic_Incognita.Image
            Case 2 : PictureBox2.Image = Pic_Incognita.Image
            Case 3 : PictureBox3.Image = Pic_Incognita.Image
            Case 4 : PictureBox4.Image = Pic_Incognita.Image
            Case 5 : PictureBox5.Image = Pic_Incognita.Image
            Case 6 : PictureBox6.Image = Pic_Incognita.Image
            Case 7 : PictureBox7.Image = Pic_Incognita.Image
            Case 8 : PictureBox8.Image = Pic_Incognita.Image
            Case 9 : PictureBox9.Image = Pic_Incognita.Image
            Case 10 : PictureBox10.Image = Pic_Incognita.Image
            Case 11 : PictureBox11.Image = Pic_Incognita.Image
            Case 12 : PictureBox12.Image = Pic_Incognita.Image
            Case 13 : PictureBox13.Image = Pic_Incognita.Image
            Case 14 : PictureBox14.Image = Pic_Incognita.Image
            Case 15 : PictureBox15.Image = Pic_Incognita.Image
            Case 16 : PictureBox16.Image = Pic_Incognita.Image
            Case 17 : PictureBox17.Image = Pic_Incognita.Image
            Case 18 : PictureBox18.Image = Pic_Incognita.Image
            Case 19 : PictureBox19.Image = Pic_Incognita.Image
            Case 20 : PictureBox20.Image = Pic_Incognita.Image
        End Select

        Select Case (eleccion_activa)
            Case 1 : PictureBox1.Image = Pic_Incognita.Image
            Case 2 : PictureBox2.Image = Pic_Incognita.Image
            Case 3 : PictureBox3.Image = Pic_Incognita.Image
            Case 4 : PictureBox4.Image = Pic_Incognita.Image
            Case 5 : PictureBox5.Image = Pic_Incognita.Image
            Case 6 : PictureBox6.Image = Pic_Incognita.Image
            Case 7 : PictureBox7.Image = Pic_Incognita.Image
            Case 8 : PictureBox8.Image = Pic_Incognita.Image
            Case 9 : PictureBox9.Image = Pic_Incognita.Image
            Case 10 : PictureBox10.Image = Pic_Incognita.Image
            Case 11 : PictureBox11.Image = Pic_Incognita.Image
            Case 12 : PictureBox12.Image = Pic_Incognita.Image
            Case 13 : PictureBox13.Image = Pic_Incognita.Image
            Case 14 : PictureBox14.Image = Pic_Incognita.Image
            Case 15 : PictureBox15.Image = Pic_Incognita.Image
            Case 16 : PictureBox16.Image = Pic_Incognita.Image
            Case 17 : PictureBox17.Image = Pic_Incognita.Image
            Case 18 : PictureBox18.Image = Pic_Incognita.Image
            Case 19 : PictureBox19.Image = Pic_Incognita.Image
            Case 20 : PictureBox20.Image = Pic_Incognita.Image
        End Select
    End Sub

    Sub Ganador()

        If P1 > P2 And P1 > P3 And P1 > P4 Then
            MsgBox("Jugador 1 Gana! con: " & P1 & " tarjetas, FELICIDADES!")
            Sc1 += 1
            lblScore1.Text = Sc1

            If P1 = P4 Or P1 = P2 Or P1 = P3 Then
                MsgBox("Nadie Gana hay un empate!")
            End If

        End If

        If P2 > P1 And P2 > P3 And P2 > P4 Then
            MsgBox("Jugador 2 Gana! con: " & P2 & " tarjetas, FELICIDADES!")
            Sc2 += 1
            lblScore2.Text = Sc2

            If P2 = P1 Or P2 = P3 Or P2 = P4 Then
                MsgBox("Nadie Gana hay un empate!")
            End If

        End If

        If P3 > P1 And P3 > P2 And P3 > P4 Then
            MsgBox("Jugador 3 Gana! con: " & P3 & " tarjetas, FELICIDADES!")
            Sc3 += 1
            lblScore3.Text = Sc3

            If P3 = P1 Or P3 = P2 Or P3 = P4 Then
                MsgBox("Nadie Gana hay un empate!")
            End If

        End If

        If P4 > P1 And P4 > P2 And P4 > P3 Then
            MsgBox("Jugador 4 Gana! con: " & P4 & " tarjetas, FELICIDADES!")
            Sc4 += 1
            lblScore4.Text = Sc4

            If P4 = P1 Or P4 = P2 Or P4 = P3 Then
                MsgBox("Nadie Gana hay un empate!")
            End If

        End If

    End Sub

    Sub ver_pareja()

        If img_nueva = img_activa Then

            Select Case TurnoActual
                Case 1
                    P1 += 1
                    lblP1.Text = P1
                Case 2
                    P2 += 1
                    lblP2.Text = P2
                Case 3
                    P3 += 1
                    lblP3.Text = P3
                Case 4
                    P4 += 1
                    lblP4.Text = P4
            End Select

            If P1 + P2 + P3 + P4 = max Then
                Ganador()
                lblTurno.Visible = False

            End If

            Select Case (eleccion_nueva)
                Case 1 : PictureBox1.Enabled = False
                Case 2 : PictureBox2.Enabled = False
                Case 3 : PictureBox3.Enabled = False
                Case 4 : PictureBox4.Enabled = False
                Case 5 : PictureBox5.Enabled = False
                Case 6 : PictureBox6.Enabled = False
                Case 7 : PictureBox7.Enabled = False
                Case 8 : PictureBox8.Enabled = False
                Case 9 : PictureBox9.Enabled = False
                Case 10 : PictureBox10.Enabled = False
                Case 11 : PictureBox11.Enabled = False
                Case 12 : PictureBox12.Enabled = False
                Case 13 : PictureBox13.Enabled = False
                Case 14 : PictureBox14.Enabled = False
                Case 15 : PictureBox15.Enabled = False
                Case 16 : PictureBox16.Enabled = False
                Case 17 : PictureBox17.Enabled = False
                Case 18 : PictureBox18.Enabled = False
                Case 19 : PictureBox19.Enabled = False
                Case 20 : PictureBox20.Enabled = False
            End Select

            Select Case (eleccion_activa)
                Case 1 : PictureBox1.Enabled = False
                Case 2 : PictureBox2.Enabled = False
                Case 3 : PictureBox3.Enabled = False
                Case 4 : PictureBox4.Enabled = False
                Case 5 : PictureBox5.Enabled = False
                Case 6 : PictureBox6.Enabled = False
                Case 7 : PictureBox7.Enabled = False
                Case 8 : PictureBox8.Enabled = False
                Case 9 : PictureBox9.Enabled = False
                Case 10 : PictureBox10.Enabled = False
                Case 11 : PictureBox11.Enabled = False
                Case 12 : PictureBox12.Enabled = False
                Case 13 : PictureBox13.Enabled = False
                Case 14 : PictureBox14.Enabled = False
                Case 15 : PictureBox15.Enabled = False
                Case 16 : PictureBox16.Enabled = False
                Case 17 : PictureBox17.Enabled = False
                Case 18 : PictureBox18.Enabled = False
                Case 19 : PictureBox19.Enabled = False
                Case 20 : PictureBox20.Enabled = False
            End Select

            'Deshabilitar todo el grupo de imágenes
        Else
            gbTarjetas.Enabled = True
            Tim_ver_parejas.Enabled = True

            TurnoActual += 1

            If TurnoActual = NumJugadores + 1 Then
                TurnoActual = 1
            End If
            Turnos()
        End If
    End Sub

    Private Sub PictureBox1_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles PictureBox1.Click
        Select Case PictureBox1.Tag
            Case "1" : PictureBox1.Image = Pic_1.Image
            Case "2" : PictureBox1.Image = Pic_2.Image
            Case "3" : PictureBox1.Image = Pic_3.Image
            Case "4" : PictureBox1.Image = Pic_4.Image
            Case "5" : PictureBox1.Image = Pic_5.Image
            Case "6" : PictureBox1.Image = Pic_6.Image
            Case "7" : PictureBox1.Image = Pic_7.Image
            Case "8" : PictureBox1.Image = Pic_8.Image
            Case "9" : PictureBox1.Image = Pic_9.Image
            Case "10" : PictureBox1.Image = Pic_10.Image
            Case "11" : PictureBox1.Image = Pic_11.Image
            Case "12" : PictureBox1.Image = Pic_12.Image
            Case "13" : PictureBox1.Image = Pic_13.Image
            Case "14" : PictureBox1.Image = Pic_14.Image
            Case "15" : PictureBox1.Image = Pic_15.Image
            Case "16" : PictureBox1.Image = Pic_16.Image
            Case "17" : PictureBox1.Image = Pic_17.Image
            Case "18" : PictureBox1.Image = Pic_18.Image
            Case "19" : PictureBox1.Image = Pic_19.Image
            Case "20" : PictureBox1.Image = Pic_20.Image
            Case "Else" : MsgBox("Ninguno" & PictureBox1.Tag)
        End Select
        If clic = 0 Then
            clic = 1
            img_activa = PictureBox1.Tag
            eleccion_activa = 1

        Else
            clic = 0
            img_nueva = PictureBox1.Tag
            eleccion_nueva = 1
            ver_pareja()
        End If
    End Sub

    Private Sub PictureBox2_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles PictureBox2.Click
        Select Case PictureBox2.Tag
            Case "1" : PictureBox2.Image = Pic_1.Image
            Case "2" : PictureBox2.Image = Pic_2.Image
            Case "3" : PictureBox2.Image = Pic_3.Image
            Case "4" : PictureBox2.Image = Pic_4.Image
            Case "5" : PictureBox2.Image = Pic_5.Image
            Case "6" : PictureBox2.Image = Pic_6.Image
            Case "7" : PictureBox2.Image = Pic_7.Image
            Case "8" : PictureBox2.Image = Pic_8.Image
            Case "9" : PictureBox2.Image = Pic_9.Image
            Case "10" : PictureBox2.Image = Pic_10.Image
            Case "11" : PictureBox2.Image = Pic_11.Image
            Case "12" : PictureBox2.Image = Pic_12.Image
            Case "13" : PictureBox2.Image = Pic_13.Image
            Case "14" : PictureBox2.Image = Pic_14.Image
            Case "15" : PictureBox2.Image = Pic_15.Image
            Case "16" : PictureBox2.Image = Pic_16.Image
            Case "17" : PictureBox2.Image = Pic_17.Image
            Case "18" : PictureBox2.Image = Pic_18.Image
            Case "19" : PictureBox2.Image = Pic_19.Image
            Case "20" : PictureBox2.Image = Pic_20.Image
            Case "Else" : MsgBox("Ninguno" & PictureBox2.Tag)
        End Select
        If clic = 0 Then
            clic = 1
            img_activa = PictureBox2.Tag
            eleccion_activa = 2

        Else
            clic = 0
            img_nueva = PictureBox2.Tag
            eleccion_nueva = 2
            ver_pareja()
        End If
    End Sub

    Private Sub PictureBox3_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles PictureBox3.Click
        Select Case PictureBox3.Tag
            Case "1" : PictureBox3.Image = Pic_1.Image
            Case "2" : PictureBox3.Image = Pic_2.Image
            Case "3" : PictureBox3.Image = Pic_3.Image
            Case "4" : PictureBox3.Image = Pic_4.Image
            Case "5" : PictureBox3.Image = Pic_5.Image
            Case "6" : PictureBox3.Image = Pic_6.Image
            Case "7" : PictureBox3.Image = Pic_7.Image
            Case "8" : PictureBox3.Image = Pic_8.Image
            Case "9" : PictureBox3.Image = Pic_9.Image
            Case "10" : PictureBox3.Image = Pic_10.Image
            Case "11" : PictureBox3.Image = Pic_11.Image
            Case "12" : PictureBox3.Image = Pic_12.Image
            Case "13" : PictureBox3.Image = Pic_13.Image
            Case "14" : PictureBox3.Image = Pic_14.Image
            Case "15" : PictureBox3.Image = Pic_15.Image
            Case "16" : PictureBox3.Image = Pic_16.Image
            Case "17" : PictureBox3.Image = Pic_17.Image
            Case "18" : PictureBox3.Image = Pic_18.Image
            Case "19" : PictureBox3.Image = Pic_19.Image
            Case "20" : PictureBox3.Image = Pic_20.Image
            Case "Else" : MsgBox("Ninguno" & PictureBox3.Tag)
        End Select
        If clic = 0 Then
            clic = 1
            img_activa = PictureBox3.Tag
            eleccion_activa = 3

        Else
            clic = 0
            img_nueva = PictureBox3.Tag
            eleccion_nueva = 3
            ver_pareja()
        End If
    End Sub

    Private Sub PictureBox4_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles PictureBox4.Click
        Select Case PictureBox4.Tag
            Case "1" : PictureBox4.Image = Pic_1.Image
            Case "2" : PictureBox4.Image = Pic_2.Image
            Case "3" : PictureBox4.Image = Pic_3.Image
            Case "4" : PictureBox4.Image = Pic_4.Image
            Case "5" : PictureBox4.Image = Pic_5.Image
            Case "6" : PictureBox4.Image = Pic_6.Image
            Case "7" : PictureBox4.Image = Pic_7.Image
            Case "8" : PictureBox4.Image = Pic_8.Image
            Case "9" : PictureBox4.Image = Pic_9.Image
            Case "10" : PictureBox4.Image = Pic_10.Image
            Case "11" : PictureBox4.Image = Pic_11.Image
            Case "12" : PictureBox4.Image = Pic_12.Image
            Case "13" : PictureBox4.Image = Pic_13.Image
            Case "14" : PictureBox4.Image = Pic_14.Image
            Case "15" : PictureBox4.Image = Pic_15.Image
            Case "16" : PictureBox4.Image = Pic_16.Image
            Case "17" : PictureBox4.Image = Pic_17.Image
            Case "18" : PictureBox4.Image = Pic_18.Image
            Case "19" : PictureBox4.Image = Pic_19.Image
            Case "20" : PictureBox4.Image = Pic_20.Image
            Case "Else" : MsgBox("Ninguno" & PictureBox4.Tag)
        End Select
        If clic = 0 Then
            clic = 1
            img_activa = PictureBox4.Tag
            eleccion_activa = 4

        Else
            clic = 0
            img_nueva = PictureBox4.Tag
            eleccion_nueva = 4
            ver_pareja()
        End If
    End Sub

    Private Sub PictureBox5_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles PictureBox5.Click
        Select Case PictureBox5.Tag
            Case "1" : PictureBox5.Image = Pic_1.Image
            Case "2" : PictureBox5.Image = Pic_2.Image
            Case "3" : PictureBox5.Image = Pic_3.Image
            Case "4" : PictureBox5.Image = Pic_4.Image
            Case "5" : PictureBox5.Image = Pic_5.Image
            Case "6" : PictureBox5.Image = Pic_6.Image
            Case "7" : PictureBox5.Image = Pic_7.Image
            Case "8" : PictureBox5.Image = Pic_8.Image
            Case "9" : PictureBox5.Image = Pic_9.Image
            Case "10" : PictureBox5.Image = Pic_10.Image
            Case "11" : PictureBox5.Image = Pic_11.Image
            Case "12" : PictureBox5.Image = Pic_12.Image
            Case "13" : PictureBox5.Image = Pic_13.Image
            Case "14" : PictureBox5.Image = Pic_14.Image
            Case "15" : PictureBox5.Image = Pic_15.Image
            Case "16" : PictureBox5.Image = Pic_16.Image
            Case "17" : PictureBox5.Image = Pic_17.Image
            Case "18" : PictureBox5.Image = Pic_18.Image
            Case "19" : PictureBox5.Image = Pic_19.Image
            Case "20" : PictureBox5.Image = Pic_20.Image
            Case "Else" : MsgBox("Ninguno" & PictureBox5.Tag)
        End Select
        If clic = 0 Then
            clic = 1
            img_activa = PictureBox5.Tag
            eleccion_activa = 5

        Else
            clic = 0
            img_nueva = PictureBox5.Tag
            eleccion_nueva = 5
            ver_pareja()
        End If
    End Sub

    Private Sub PictureBox6_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles PictureBox6.Click
        Select Case PictureBox6.Tag
            Case "1" : PictureBox6.Image = Pic_1.Image
            Case "2" : PictureBox6.Image = Pic_2.Image
            Case "3" : PictureBox6.Image = Pic_3.Image
            Case "4" : PictureBox6.Image = Pic_4.Image
            Case "5" : PictureBox6.Image = Pic_5.Image
            Case "6" : PictureBox6.Image = Pic_6.Image
            Case "7" : PictureBox6.Image = Pic_7.Image
            Case "8" : PictureBox6.Image = Pic_8.Image
            Case "9" : PictureBox6.Image = Pic_9.Image
            Case "10" : PictureBox6.Image = Pic_10.Image
            Case "11" : PictureBox6.Image = Pic_11.Image
            Case "12" : PictureBox6.Image = Pic_12.Image
            Case "13" : PictureBox6.Image = Pic_13.Image
            Case "14" : PictureBox6.Image = Pic_14.Image
            Case "15" : PictureBox6.Image = Pic_15.Image
            Case "16" : PictureBox6.Image = Pic_16.Image
            Case "17" : PictureBox6.Image = Pic_17.Image
            Case "18" : PictureBox6.Image = Pic_18.Image
            Case "19" : PictureBox6.Image = Pic_19.Image
            Case "20" : PictureBox6.Image = Pic_20.Image
            Case "Else" : MsgBox("Ninguno" & PictureBox6.Tag)
        End Select
        If clic = 0 Then
            clic = 1
            img_activa = PictureBox6.Tag
            eleccion_activa = 6

        Else
            clic = 0
            img_nueva = PictureBox6.Tag
            eleccion_nueva = 6
            ver_pareja()
        End If
    End Sub

    Private Sub PictureBox7_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles PictureBox7.Click
        Select Case PictureBox7.Tag
            Case "1" : PictureBox7.Image = Pic_1.Image
            Case "2" : PictureBox7.Image = Pic_2.Image
            Case "3" : PictureBox7.Image = Pic_3.Image
            Case "4" : PictureBox7.Image = Pic_4.Image
            Case "5" : PictureBox7.Image = Pic_5.Image
            Case "6" : PictureBox7.Image = Pic_6.Image
            Case "7" : PictureBox7.Image = Pic_7.Image
            Case "8" : PictureBox7.Image = Pic_8.Image
            Case "9" : PictureBox7.Image = Pic_9.Image
            Case "10" : PictureBox7.Image = Pic_10.Image
            Case "11" : PictureBox7.Image = Pic_11.Image
            Case "12" : PictureBox7.Image = Pic_12.Image
            Case "13" : PictureBox7.Image = Pic_13.Image
            Case "14" : PictureBox7.Image = Pic_14.Image
            Case "15" : PictureBox7.Image = Pic_15.Image
            Case "16" : PictureBox7.Image = Pic_16.Image
            Case "17" : PictureBox7.Image = Pic_17.Image
            Case "18" : PictureBox7.Image = Pic_18.Image
            Case "19" : PictureBox7.Image = Pic_19.Image
            Case "20" : PictureBox7.Image = Pic_20.Image
            Case "Else" : MsgBox("Ninguno" & PictureBox7.Tag)
        End Select
        If clic = 0 Then
            clic = 1
            img_activa = PictureBox7.Tag
            eleccion_activa = 7

        Else
            clic = 0
            img_nueva = PictureBox7.Tag
            eleccion_nueva = 7
            ver_pareja()
        End If
    End Sub

    Private Sub PictureBox8_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles PictureBox8.Click
        Select Case PictureBox8.Tag
            Case "1" : PictureBox8.Image = Pic_1.Image
            Case "2" : PictureBox8.Image = Pic_2.Image
            Case "3" : PictureBox8.Image = Pic_3.Image
            Case "4" : PictureBox8.Image = Pic_4.Image
            Case "5" : PictureBox8.Image = Pic_5.Image
            Case "6" : PictureBox8.Image = Pic_6.Image
            Case "7" : PictureBox8.Image = Pic_7.Image
            Case "8" : PictureBox8.Image = Pic_8.Image
            Case "9" : PictureBox8.Image = Pic_9.Image
            Case "10" : PictureBox8.Image = Pic_10.Image
            Case "11" : PictureBox8.Image = Pic_11.Image
            Case "12" : PictureBox8.Image = Pic_12.Image
            Case "13" : PictureBox8.Image = Pic_13.Image
            Case "14" : PictureBox8.Image = Pic_14.Image
            Case "15" : PictureBox8.Image = Pic_15.Image
            Case "16" : PictureBox8.Image = Pic_16.Image
            Case "17" : PictureBox8.Image = Pic_17.Image
            Case "18" : PictureBox8.Image = Pic_18.Image
            Case "19" : PictureBox8.Image = Pic_19.Image
            Case "20" : PictureBox8.Image = Pic_20.Image
            Case "Else" : MsgBox("Ninguno" & PictureBox8.Tag)
        End Select
        If clic = 0 Then
            clic = 1
            img_activa = PictureBox8.Tag
            eleccion_activa = 8

        Else
            clic = 0
            img_nueva = PictureBox8.Tag
            eleccion_nueva = 8
            ver_pareja()
        End If
    End Sub

    Private Sub PictureBox9_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles PictureBox9.Click
        Select Case PictureBox9.Tag
            Case "1" : PictureBox9.Image = Pic_1.Image
            Case "2" : PictureBox9.Image = Pic_2.Image
            Case "3" : PictureBox9.Image = Pic_3.Image
            Case "4" : PictureBox9.Image = Pic_4.Image
            Case "5" : PictureBox9.Image = Pic_5.Image
            Case "6" : PictureBox9.Image = Pic_6.Image
            Case "7" : PictureBox9.Image = Pic_7.Image
            Case "8" : PictureBox9.Image = Pic_8.Image
            Case "9" : PictureBox9.Image = Pic_9.Image
            Case "10" : PictureBox9.Image = Pic_10.Image
            Case "11" : PictureBox9.Image = Pic_11.Image
            Case "12" : PictureBox9.Image = Pic_12.Image
            Case "13" : PictureBox9.Image = Pic_13.Image
            Case "14" : PictureBox9.Image = Pic_14.Image
            Case "15" : PictureBox9.Image = Pic_15.Image
            Case "16" : PictureBox9.Image = Pic_16.Image
            Case "17" : PictureBox9.Image = Pic_17.Image
            Case "18" : PictureBox9.Image = Pic_18.Image
            Case "19" : PictureBox9.Image = Pic_19.Image
            Case "20" : PictureBox9.Image = Pic_20.Image
            Case "Else" : MsgBox("Ninguno" & PictureBox9.Tag)
        End Select
        If clic = 0 Then
            clic = 1
            img_activa = PictureBox9.Tag
            eleccion_activa = 9

        Else
            clic = 0
            img_nueva = PictureBox9.Tag
            eleccion_nueva = 9
            ver_pareja()
        End If
    End Sub

    Private Sub PictureBox10_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles PictureBox10.Click
        Select Case PictureBox10.Tag
            Case "1" : PictureBox10.Image = Pic_1.Image
            Case "2" : PictureBox10.Image = Pic_2.Image
            Case "3" : PictureBox10.Image = Pic_3.Image
            Case "4" : PictureBox10.Image = Pic_4.Image
            Case "5" : PictureBox10.Image = Pic_5.Image
            Case "6" : PictureBox10.Image = Pic_6.Image
            Case "7" : PictureBox10.Image = Pic_7.Image
            Case "8" : PictureBox10.Image = Pic_8.Image
            Case "9" : PictureBox10.Image = Pic_9.Image
            Case "10" : PictureBox10.Image = Pic_10.Image
            Case "11" : PictureBox10.Image = Pic_11.Image
            Case "12" : PictureBox10.Image = Pic_12.Image
            Case "13" : PictureBox10.Image = Pic_13.Image
            Case "14" : PictureBox10.Image = Pic_14.Image
            Case "15" : PictureBox10.Image = Pic_15.Image
            Case "16" : PictureBox10.Image = Pic_16.Image
            Case "17" : PictureBox10.Image = Pic_17.Image
            Case "18" : PictureBox10.Image = Pic_18.Image
            Case "19" : PictureBox10.Image = Pic_19.Image
            Case "20" : PictureBox10.Image = Pic_20.Image
            Case "Else" : MsgBox("Ninguno" & PictureBox10.Tag)
        End Select
        If clic = 0 Then
            clic = 1
            img_activa = PictureBox10.Tag
            eleccion_activa = 10

        Else
            clic = 0
            img_nueva = PictureBox10.Tag
            eleccion_nueva = 10
            ver_pareja()
        End If
    End Sub

    Private Sub PictureBox11_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles PictureBox11.Click
        Select Case PictureBox11.Tag
            Case "1" : PictureBox11.Image = Pic_1.Image
            Case "2" : PictureBox11.Image = Pic_2.Image
            Case "3" : PictureBox11.Image = Pic_3.Image
            Case "4" : PictureBox11.Image = Pic_4.Image
            Case "5" : PictureBox11.Image = Pic_5.Image
            Case "6" : PictureBox11.Image = Pic_6.Image
            Case "7" : PictureBox11.Image = Pic_7.Image
            Case "8" : PictureBox11.Image = Pic_8.Image
            Case "9" : PictureBox11.Image = Pic_9.Image
            Case "10" : PictureBox11.Image = Pic_10.Image
            Case "11" : PictureBox11.Image = Pic_11.Image
            Case "12" : PictureBox11.Image = Pic_12.Image
            Case "13" : PictureBox11.Image = Pic_13.Image
            Case "14" : PictureBox11.Image = Pic_14.Image
            Case "15" : PictureBox11.Image = Pic_15.Image
            Case "16" : PictureBox11.Image = Pic_16.Image
            Case "17" : PictureBox11.Image = Pic_17.Image
            Case "18" : PictureBox11.Image = Pic_18.Image
            Case "19" : PictureBox11.Image = Pic_19.Image
            Case "20" : PictureBox11.Image = Pic_20.Image
            Case "Else" : MsgBox("Ninguno" & PictureBox11.Tag)
        End Select
        If clic = 0 Then
            clic = 1
            img_activa = PictureBox11.Tag
            eleccion_activa = 11

        Else
            clic = 0
            img_nueva = PictureBox11.Tag
            eleccion_nueva = 11
            ver_pareja()
        End If
    End Sub

    Private Sub PictureBox12_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles PictureBox12.Click
        Select Case PictureBox12.Tag
            Case "1" : PictureBox12.Image = Pic_1.Image
            Case "2" : PictureBox12.Image = Pic_2.Image
            Case "3" : PictureBox12.Image = Pic_3.Image
            Case "4" : PictureBox12.Image = Pic_4.Image
            Case "5" : PictureBox12.Image = Pic_5.Image
            Case "6" : PictureBox12.Image = Pic_6.Image
            Case "7" : PictureBox12.Image = Pic_7.Image
            Case "8" : PictureBox12.Image = Pic_8.Image
            Case "9" : PictureBox12.Image = Pic_9.Image
            Case "10" : PictureBox12.Image = Pic_10.Image
            Case "11" : PictureBox12.Image = Pic_11.Image
            Case "12" : PictureBox12.Image = Pic_12.Image
            Case "13" : PictureBox12.Image = Pic_13.Image
            Case "14" : PictureBox12.Image = Pic_14.Image
            Case "15" : PictureBox12.Image = Pic_15.Image
            Case "16" : PictureBox12.Image = Pic_16.Image
            Case "17" : PictureBox12.Image = Pic_17.Image
            Case "18" : PictureBox12.Image = Pic_18.Image
            Case "19" : PictureBox12.Image = Pic_19.Image
            Case "20" : PictureBox12.Image = Pic_20.Image
            Case "Else" : MsgBox("Ninguno" & PictureBox12.Tag)
        End Select
        If clic = 0 Then
            clic = 1
            img_activa = PictureBox12.Tag
            eleccion_activa = 12

        Else
            clic = 0
            img_nueva = PictureBox12.Tag
            eleccion_nueva = 12
            ver_pareja()
        End If
    End Sub

    Private Sub PictureBox13_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles PictureBox13.Click
        Select Case PictureBox13.Tag
            Case "1" : PictureBox13.Image = Pic_1.Image
            Case "2" : PictureBox13.Image = Pic_2.Image
            Case "3" : PictureBox13.Image = Pic_3.Image
            Case "4" : PictureBox13.Image = Pic_4.Image
            Case "5" : PictureBox13.Image = Pic_5.Image
            Case "6" : PictureBox13.Image = Pic_6.Image
            Case "7" : PictureBox13.Image = Pic_7.Image
            Case "8" : PictureBox13.Image = Pic_8.Image
            Case "9" : PictureBox13.Image = Pic_9.Image
            Case "10" : PictureBox13.Image = Pic_10.Image
            Case "11" : PictureBox13.Image = Pic_11.Image
            Case "12" : PictureBox13.Image = Pic_12.Image
            Case "13" : PictureBox13.Image = Pic_13.Image
            Case "14" : PictureBox13.Image = Pic_14.Image
            Case "15" : PictureBox13.Image = Pic_15.Image
            Case "16" : PictureBox13.Image = Pic_16.Image
            Case "17" : PictureBox13.Image = Pic_17.Image
            Case "18" : PictureBox13.Image = Pic_18.Image
            Case "19" : PictureBox13.Image = Pic_19.Image
            Case "20" : PictureBox13.Image = Pic_20.Image
            Case "Else" : MsgBox("Ninguno" & PictureBox13.Tag)
        End Select
        If clic = 0 Then
            clic = 1
            img_activa = PictureBox13.Tag
            eleccion_activa = 13

        Else
            clic = 0
            img_nueva = PictureBox13.Tag
            eleccion_nueva = 13
            ver_pareja()
        End If
    End Sub

    Private Sub PictureBox14_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles PictureBox14.Click
        Select Case PictureBox14.Tag
            Case "1" : PictureBox14.Image = Pic_1.Image
            Case "2" : PictureBox14.Image = Pic_2.Image
            Case "3" : PictureBox14.Image = Pic_3.Image
            Case "4" : PictureBox14.Image = Pic_4.Image
            Case "5" : PictureBox14.Image = Pic_5.Image
            Case "6" : PictureBox14.Image = Pic_6.Image
            Case "7" : PictureBox14.Image = Pic_7.Image
            Case "8" : PictureBox14.Image = Pic_8.Image
            Case "9" : PictureBox14.Image = Pic_9.Image
            Case "10" : PictureBox14.Image = Pic_10.Image
            Case "11" : PictureBox14.Image = Pic_11.Image
            Case "12" : PictureBox14.Image = Pic_12.Image
            Case "13" : PictureBox14.Image = Pic_13.Image
            Case "14" : PictureBox14.Image = Pic_14.Image
            Case "15" : PictureBox14.Image = Pic_15.Image
            Case "16" : PictureBox14.Image = Pic_16.Image
            Case "17" : PictureBox14.Image = Pic_17.Image
            Case "18" : PictureBox14.Image = Pic_18.Image
            Case "19" : PictureBox14.Image = Pic_19.Image
            Case "20" : PictureBox14.Image = Pic_20.Image
            Case "Else" : MsgBox("Ninguno" & PictureBox14.Tag)
        End Select
        If clic = 0 Then
            clic = 1
            img_activa = PictureBox14.Tag
            eleccion_activa = 14

        Else
            clic = 0
            img_nueva = PictureBox14.Tag
            eleccion_nueva = 14
            ver_pareja()
        End If
    End Sub

    Private Sub PictureBox15_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles PictureBox15.Click
        Select Case PictureBox15.Tag
            Case "1" : PictureBox15.Image = Pic_1.Image
            Case "2" : PictureBox15.Image = Pic_2.Image
            Case "3" : PictureBox15.Image = Pic_3.Image
            Case "4" : PictureBox15.Image = Pic_4.Image
            Case "5" : PictureBox15.Image = Pic_5.Image
            Case "6" : PictureBox15.Image = Pic_6.Image
            Case "7" : PictureBox15.Image = Pic_7.Image
            Case "8" : PictureBox15.Image = Pic_8.Image
            Case "9" : PictureBox15.Image = Pic_9.Image
            Case "10" : PictureBox15.Image = Pic_10.Image
            Case "11" : PictureBox15.Image = Pic_11.Image
            Case "12" : PictureBox15.Image = Pic_12.Image
            Case "13" : PictureBox15.Image = Pic_13.Image
            Case "14" : PictureBox15.Image = Pic_14.Image
            Case "15" : PictureBox15.Image = Pic_15.Image
            Case "16" : PictureBox15.Image = Pic_16.Image
            Case "17" : PictureBox15.Image = Pic_17.Image
            Case "18" : PictureBox15.Image = Pic_18.Image
            Case "19" : PictureBox15.Image = Pic_19.Image
            Case "20" : PictureBox15.Image = Pic_20.Image
            Case "Else" : MsgBox("Ninguno" & PictureBox15.Tag)
        End Select
        If clic = 0 Then
            clic = 1
            img_activa = PictureBox15.Tag
            eleccion_activa = 15

        Else
            clic = 0
            img_nueva = PictureBox15.Tag
            eleccion_nueva = 15
            ver_pareja()
        End If
    End Sub

    Private Sub PictureBox16_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles PictureBox16.Click
        Select Case PictureBox16.Tag
            Case "1" : PictureBox16.Image = Pic_1.Image
            Case "2" : PictureBox16.Image = Pic_2.Image
            Case "3" : PictureBox16.Image = Pic_3.Image
            Case "4" : PictureBox16.Image = Pic_4.Image
            Case "5" : PictureBox16.Image = Pic_5.Image
            Case "6" : PictureBox16.Image = Pic_6.Image
            Case "7" : PictureBox16.Image = Pic_7.Image
            Case "8" : PictureBox16.Image = Pic_8.Image
            Case "9" : PictureBox16.Image = Pic_9.Image
            Case "10" : PictureBox16.Image = Pic_10.Image
            Case "11" : PictureBox16.Image = Pic_11.Image
            Case "12" : PictureBox16.Image = Pic_12.Image
            Case "13" : PictureBox16.Image = Pic_13.Image
            Case "14" : PictureBox16.Image = Pic_14.Image
            Case "15" : PictureBox16.Image = Pic_15.Image
            Case "16" : PictureBox16.Image = Pic_16.Image
            Case "17" : PictureBox16.Image = Pic_17.Image
            Case "18" : PictureBox16.Image = Pic_18.Image
            Case "19" : PictureBox16.Image = Pic_19.Image
            Case "20" : PictureBox16.Image = Pic_20.Image
            Case "Else" : MsgBox("Ninguno" & PictureBox16.Tag)
        End Select
        If clic = 0 Then
            clic = 1
            img_activa = PictureBox16.Tag
            eleccion_activa = 16

        Else
            clic = 0
            img_nueva = PictureBox16.Tag
            eleccion_nueva = 16
            ver_pareja()
        End If
    End Sub

    Private Sub PictureBox17_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles PictureBox17.Click
        Select Case PictureBox17.Tag
            Case "1" : PictureBox17.Image = Pic_1.Image
            Case "2" : PictureBox17.Image = Pic_2.Image
            Case "3" : PictureBox17.Image = Pic_3.Image
            Case "4" : PictureBox17.Image = Pic_4.Image
            Case "5" : PictureBox17.Image = Pic_5.Image
            Case "6" : PictureBox17.Image = Pic_6.Image
            Case "7" : PictureBox17.Image = Pic_7.Image
            Case "8" : PictureBox17.Image = Pic_8.Image
            Case "9" : PictureBox17.Image = Pic_9.Image
            Case "10" : PictureBox17.Image = Pic_10.Image
            Case "11" : PictureBox17.Image = Pic_11.Image
            Case "12" : PictureBox17.Image = Pic_12.Image
            Case "13" : PictureBox17.Image = Pic_13.Image
            Case "14" : PictureBox17.Image = Pic_14.Image
            Case "15" : PictureBox17.Image = Pic_15.Image
            Case "16" : PictureBox17.Image = Pic_16.Image
            Case "17" : PictureBox17.Image = Pic_17.Image
            Case "18" : PictureBox17.Image = Pic_18.Image
            Case "19" : PictureBox17.Image = Pic_19.Image
            Case "20" : PictureBox17.Image = Pic_20.Image
            Case "Else" : MsgBox("Ninguno" & PictureBox17.Tag)
        End Select
        If clic = 0 Then
            clic = 1
            img_activa = PictureBox17.Tag
            eleccion_activa = 17

        Else
            clic = 0
            img_nueva = PictureBox17.Tag
            eleccion_nueva = 17
            ver_pareja()
        End If
    End Sub

    Private Sub PictureBox18_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles PictureBox18.Click
        Select Case PictureBox18.Tag
            Case "1" : PictureBox18.Image = Pic_1.Image
            Case "2" : PictureBox18.Image = Pic_2.Image
            Case "3" : PictureBox18.Image = Pic_3.Image
            Case "4" : PictureBox18.Image = Pic_4.Image
            Case "5" : PictureBox18.Image = Pic_5.Image
            Case "6" : PictureBox18.Image = Pic_6.Image
            Case "7" : PictureBox18.Image = Pic_7.Image
            Case "8" : PictureBox18.Image = Pic_8.Image
            Case "9" : PictureBox18.Image = Pic_9.Image
            Case "10" : PictureBox18.Image = Pic_10.Image
            Case "11" : PictureBox18.Image = Pic_11.Image
            Case "12" : PictureBox18.Image = Pic_12.Image
            Case "13" : PictureBox18.Image = Pic_13.Image
            Case "14" : PictureBox18.Image = Pic_14.Image
            Case "15" : PictureBox18.Image = Pic_15.Image
            Case "16" : PictureBox18.Image = Pic_16.Image
            Case "17" : PictureBox18.Image = Pic_17.Image
            Case "18" : PictureBox18.Image = Pic_18.Image
            Case "19" : PictureBox18.Image = Pic_19.Image
            Case "20" : PictureBox18.Image = Pic_20.Image
            Case "Else" : MsgBox("Ninguno" & PictureBox18.Tag)
        End Select
        If clic = 0 Then
            clic = 1
            img_activa = PictureBox18.Tag
            eleccion_activa = 18

        Else
            clic = 0
            img_nueva = PictureBox18.Tag
            eleccion_nueva = 18
            ver_pareja()
        End If
    End Sub

    Private Sub PictureBox19_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles PictureBox19.Click
        Select Case PictureBox19.Tag
            Case "1" : PictureBox19.Image = Pic_1.Image
            Case "2" : PictureBox19.Image = Pic_2.Image
            Case "3" : PictureBox19.Image = Pic_3.Image
            Case "4" : PictureBox19.Image = Pic_4.Image
            Case "5" : PictureBox19.Image = Pic_5.Image
            Case "6" : PictureBox19.Image = Pic_6.Image
            Case "7" : PictureBox19.Image = Pic_7.Image
            Case "8" : PictureBox19.Image = Pic_8.Image
            Case "9" : PictureBox19.Image = Pic_9.Image
            Case "10" : PictureBox19.Image = Pic_10.Image
            Case "11" : PictureBox19.Image = Pic_11.Image
            Case "12" : PictureBox19.Image = Pic_12.Image
            Case "13" : PictureBox19.Image = Pic_13.Image
            Case "14" : PictureBox19.Image = Pic_14.Image
            Case "15" : PictureBox19.Image = Pic_15.Image
            Case "16" : PictureBox19.Image = Pic_16.Image
            Case "17" : PictureBox19.Image = Pic_17.Image
            Case "18" : PictureBox19.Image = Pic_18.Image
            Case "19" : PictureBox19.Image = Pic_19.Image
            Case "20" : PictureBox19.Image = Pic_20.Image
            Case "Else" : MsgBox("Ninguno" & PictureBox19.Tag)
        End Select
        If clic = 0 Then
            clic = 1
            img_activa = PictureBox19.Tag
            eleccion_activa = 19

        Else
            clic = 0
            img_nueva = PictureBox19.Tag
            eleccion_nueva = 19
            ver_pareja()
        End If
    End Sub

    Private Sub PictureBox20_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles PictureBox20.Click
        Select Case PictureBox20.Tag
            Case "1" : PictureBox20.Image = Pic_1.Image
            Case "2" : PictureBox20.Image = Pic_2.Image
            Case "3" : PictureBox20.Image = Pic_3.Image
            Case "4" : PictureBox20.Image = Pic_4.Image
            Case "5" : PictureBox20.Image = Pic_5.Image
            Case "6" : PictureBox20.Image = Pic_6.Image
            Case "7" : PictureBox20.Image = Pic_7.Image
            Case "8" : PictureBox20.Image = Pic_8.Image
            Case "9" : PictureBox20.Image = Pic_9.Image
            Case "10" : PictureBox20.Image = Pic_10.Image
            Case "11" : PictureBox20.Image = Pic_11.Image
            Case "12" : PictureBox20.Image = Pic_12.Image
            Case "13" : PictureBox20.Image = Pic_13.Image
            Case "14" : PictureBox20.Image = Pic_14.Image
            Case "15" : PictureBox20.Image = Pic_15.Image
            Case "16" : PictureBox20.Image = Pic_16.Image
            Case "17" : PictureBox20.Image = Pic_17.Image
            Case "18" : PictureBox20.Image = Pic_18.Image
            Case "19" : PictureBox20.Image = Pic_19.Image
            Case "20" : PictureBox20.Image = Pic_20.Image
            Case "Else" : MsgBox("Ninguno" & PictureBox20.Tag)
        End Select
        If clic = 0 Then
            clic = 1
            img_activa = PictureBox20.Tag
            eleccion_activa = 20

        Else
            clic = 0
            img_nueva = PictureBox20.Tag
            eleccion_nueva = 20
            ver_pareja()
        End If
    End Sub

    Private Sub btnIniciar_Click(sender As Object, e As EventArgs) Handles btnIniciar.Click

        gbJugadores.Enabled = False
        Habilitar()
        Deshabilitar()

        btnIniciar.Enabled = False
        btnReiniciar.Enabled = True

        TurnoActual = 1
        lblTurno.Visible = True


        If btnIniciar.Enabled = False Then

            PictureBox1.Enabled = True
            PictureBox2.Enabled = True
            PictureBox3.Enabled = True
            PictureBox4.Enabled = True
            PictureBox5.Enabled = True
            PictureBox6.Enabled = True
            PictureBox7.Enabled = True
            PictureBox8.Enabled = True
            PictureBox9.Enabled = True
            PictureBox10.Enabled = True
            PictureBox11.Enabled = True
            PictureBox12.Enabled = True
            PictureBox13.Enabled = True
            PictureBox14.Enabled = True
            PictureBox15.Enabled = True
            PictureBox16.Enabled = True
            PictureBox17.Enabled = True
            PictureBox18.Enabled = True
            PictureBox19.Enabled = True
            PictureBox20.Enabled = True

            PictureBox1.Image = Pic_Incognita.Image
            PictureBox2.Image = Pic_Incognita.Image
            PictureBox3.Image = Pic_Incognita.Image
            PictureBox4.Image = Pic_Incognita.Image
            PictureBox5.Image = Pic_Incognita.Image
            PictureBox6.Image = Pic_Incognita.Image
            PictureBox7.Image = Pic_Incognita.Image
            PictureBox8.Image = Pic_Incognita.Image
            PictureBox9.Image = Pic_Incognita.Image
            PictureBox10.Image = Pic_Incognita.Image
            PictureBox11.Image = Pic_Incognita.Image
            PictureBox12.Image = Pic_Incognita.Image
            PictureBox13.Image = Pic_Incognita.Image
            PictureBox14.Image = Pic_Incognita.Image
            PictureBox15.Image = Pic_Incognita.Image
            PictureBox16.Image = Pic_Incognita.Image
            PictureBox17.Image = Pic_Incognita.Image
            PictureBox18.Image = Pic_Incognita.Image
            PictureBox19.Image = Pic_Incognita.Image
            PictureBox20.Image = Pic_Incognita.Image

            PictureBox1.Show()
            PictureBox2.Show()
            PictureBox3.Show()
            PictureBox4.Show()
            PictureBox5.Show()
            PictureBox6.Show()
            PictureBox7.Show()
            PictureBox8.Show()
            PictureBox9.Show()
            PictureBox10.Show()
            PictureBox11.Show()
            PictureBox12.Show()
            PictureBox13.Show()
            PictureBox14.Show()
            PictureBox15.Show()
            PictureBox16.Show()
            PictureBox17.Show()
            PictureBox18.Show()
            PictureBox19.Show()
            PictureBox20.Show()

            Randomize()
            Dim i, j As Integer
            Dim pareja As Integer
            Dim conta As Integer
            Dim vec(30) As Integer
            ListBox1.Items.Clear()
            max = 10
            For i = 1 To max * 2
                vec(i) = 0
            Next

            i = 1
            Do While i <= max * 2
                pareja = Rnd() * max + 1
                If pareja <= max Then
                    conta = 0
                    For j = 1 To max * 2
                        If vec(j) = pareja Then
                            conta = conta + 1
                        End If
                    Next
                    If conta < 2 Then
                        vec(i) = pareja
                        ListBox1.Items.Add(i & " " & vec(i) & " con " & conta)
                        i = i + 1
                    End If
                End If
            Loop

            PictureBox1.Tag = vec(1)
            PictureBox2.Tag = vec(2)
            PictureBox3.Tag = vec(3)
            PictureBox4.Tag = vec(4)
            PictureBox5.Tag = vec(5)
            PictureBox6.Tag = vec(6)
            PictureBox7.Tag = vec(7)
            PictureBox8.Tag = vec(8)
            PictureBox9.Tag = vec(9)
            PictureBox10.Tag = vec(10)
            PictureBox11.Tag = vec(11)
            PictureBox12.Tag = vec(12)
            PictureBox13.Tag = vec(13)
            PictureBox14.Tag = vec(14)
            PictureBox15.Tag = vec(15)
            PictureBox16.Tag = vec(16)
            PictureBox17.Tag = vec(17)
            PictureBox18.Tag = vec(18)
            PictureBox19.Tag = vec(19)
            PictureBox20.Tag = vec(20)

            clic = 0
            P1 = 0
            P2 = 0
            P3 = 0
            P4 = 0
            max = 10

            gbTarjetas.Enabled = True

        End If
    End Sub

    Public Sub Habilitar()
        lblJ1.Enabled = True
        lblP1.Enabled = True
        lblP1.Text = "0"

        lblJ2.Enabled = True
        lblP2.Enabled = True
        lblP2.Text = "0"

        lblJ3.Enabled = True
        lblP3.Enabled = True
        lblP3.Text = "0"

        lblJ4.Enabled = True
        lblP4.Enabled = True
        lblP4.Text = "0"
    End Sub

    Public Sub Deshabilitar()
        Select Case NumJugadores
            Case 0
                lblJ1.Enabled = False
                lblP1.Enabled = False
                lblP1.Text = "---"

                lblJ2.Enabled = False
                lblP2.Enabled = False
                lblP2.Text = "---"

                lblJ3.Enabled = False
                lblP3.Enabled = False
                lblP3.Text = "---"

                lblJ4.Enabled = False
                lblP4.Enabled = False
                lblP4.Text = "---"

            Case 1
                lblJ2.Enabled = False
                lblP2.Enabled = False
                lblP2.Text = "---"

                lblJ3.Enabled = False
                lblP3.Enabled = False
                lblP3.Text = "---"

                lblJ4.Enabled = False
                lblP4.Enabled = False
                lblP4.Text = "---"

            Case 2
                lblJ3.Enabled = False
                lblP3.Enabled = False
                lblP3.Text = "---"

                lblJ4.Enabled = False
                lblP4.Enabled = False
                lblP4.Text = "---"

            Case 3
                lblJ4.Enabled = False
                lblP4.Enabled = False
                lblP4.Text = "---"

        End Select
    End Sub

    Private Sub rd1J_CheckedChanged(sender As Object, e As EventArgs) Handles rd1J.CheckedChanged
        If rd1J.Checked = True Then
            NumJugadores = 1
        End If
    End Sub

    Private Sub rd2J_CheckedChanged(sender As Object, e As EventArgs) Handles rd2J.CheckedChanged
        If rd2J.Checked = True Then
            NumJugadores = 2
        End If
    End Sub

    Private Sub rd3J_CheckedChanged(sender As Object, e As EventArgs) Handles rd3J.CheckedChanged
        If rd3J.Checked = True Then
            NumJugadores = 3
        End If
    End Sub

    Private Sub rd4J_CheckedChanged(sender As Object, e As EventArgs) Handles rd4J.CheckedChanged
        If rd4J.Checked = True Then
            NumJugadores = 4
        End If
    End Sub

    Public Sub Turnos()


        Select Case TurnoActual
            Case 1

                lblTurno.Text = "Turno del Jugador 1"

            Case 2

                lblTurno.Text = "Turno del Jugador 2"

            Case 3

                lblTurno.Text = "Turno del Jugador 3"

            Case 4

                lblTurno.Text = "Turno del Jugador 4"


        End Select
    End Sub

    Private Sub Label1_Click(sender As Object, e As EventArgs) Handles Label1.Click

    End Sub

    Private Sub btnReiniciar_Click(sender As Object, e As EventArgs) Handles btnReiniciar.Click

        gbJugadores.Enabled = True

        rd1J.Checked = False
        rd2J.Checked = False
        rd3J.Checked = False
        rd4J.Checked = False

        NumJugadores = 0

        btnIniciar.Enabled = True
        btnReiniciar.Enabled = False

        Habilitar()
        Deshabilitar()

        lblTurno.Visible = False
        lblTurno.Text = "Turno del Jugador 1"

        gbTarjetas.Enabled = False

        TurnoActual = 1

        P1 = 0
        P2 = 0
        P3 = 0
        P4 = 0

        PictureBox1.Image = Pic_Incognita.Image
        PictureBox2.Image = Pic_Incognita.Image
        PictureBox3.Image = Pic_Incognita.Image
        PictureBox4.Image = Pic_Incognita.Image
        PictureBox5.Image = Pic_Incognita.Image
        PictureBox6.Image = Pic_Incognita.Image
        PictureBox7.Image = Pic_Incognita.Image
        PictureBox8.Image = Pic_Incognita.Image
        PictureBox9.Image = Pic_Incognita.Image
        PictureBox10.Image = Pic_Incognita.Image
        PictureBox11.Image = Pic_Incognita.Image
        PictureBox12.Image = Pic_Incognita.Image
        PictureBox13.Image = Pic_Incognita.Image
        PictureBox14.Image = Pic_Incognita.Image
        PictureBox15.Image = Pic_Incognita.Image
        PictureBox16.Image = Pic_Incognita.Image
        PictureBox17.Image = Pic_Incognita.Image
        PictureBox18.Image = Pic_Incognita.Image
        PictureBox19.Image = Pic_Incognita.Image
        PictureBox20.Image = Pic_Incognita.Image

    End Sub


    Private Sub GroupBox1_Enter(sender As Object, e As EventArgs) Handles GroupBox1.Enter

    End Sub
End Class
