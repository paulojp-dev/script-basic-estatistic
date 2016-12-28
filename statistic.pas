(*
* Author: João Paulo Ferreira da Silva (PauloJP)
* Date: 08/2014
*
* DESCRIPTION:
* Script para Matéria de Estatística, capaz de calcular: 
* 	- Média Processo Normal;
* 	- Média Processo Breve;
*	- Mediana;
* 	- Calcular o Quartil;
* 	- Percentil;
* 	- Desvio Padrão Processo Normal;
* 	- Desvio Padrão Processo Breve;
* 	- Coeficiente de Variação;
* 	- Coeficiente de Assimetria;
* 	- Coeficiente de Curtose.
*)
Program estatistica;
USES crt;
Const
	max = 12;
Type
	vetor_classe = array[1..max] OF Longint;
	classes = record
		linf : vetor_classe;
		lsup : vetor_classe;
		fi : vetor_classe;
	end;
	quartil = record
		l : Integer;
		Fzi_ant : Integer;
		f : Integer;
	end;
	
Var
	classe : classes;
	qk, pk : quartil; 
	fiyi2, yi2, fixi2, xi2, Fzi, fiyi, yi, xi, fixi : vetor_classe;
	k, opc, n, f, l, Fzi_ant, mo, s_fiyi, h, xo, s_fi, s_fixi, i, amp, q_classe : Longint;
	c, q3, q1, p90, p10, r_as, cv, s, r_p, termo_percentil, r_q, termo_quartil, s_fiyi2, s_fixi2, md, r_med_normal, r_med_breve : Real;
	
	//FUNÇÕES E PROCEDIMENTOS:
	
	//Calculando o xi
	FUNCTION calc_xi(classe : classes; q_classe : Integer) : vetor_classe;
	begin
		FOR i := 1 TO q_classe DO 
			xi[i] := (classe.linf[i] + classe.lsup[i]) div 2;
		calc_xi := xi;
	end;
	//Somando o fi
	FUNCTION soma_fi(classe : classes; q_classe : Integer) : Integer;
	var
		s_fi : Integer;
	begin
		s_fi := 0;
		FOR i := 1 TO q_classe DO
			s_fi := s_fi + classe.fi[i];
		soma_fi := s_fi;
	end;
	//Calculando o fixi
	FUNCTION calc_fixi(classe : classes; xi : vetor_classe; q_classe : Integer) : vetor_classe;
	begin
		FOR i := 1 TO q_classe DO
			fixi[i] := classe.fi[i] * xi[i];
		calc_fixi := fixi;
	end;
	//Somando o fixi
	FUNCTION soma_fixi(fixi : vetor_classe; q_classe : Integer) : Integer;
	var
		s_fixi : Integer;
	begin
		s_fixi := 0;
		FOR i := 1 TO q_classe DO
			INC(s_fixi, fixi[i]);
		soma_fixi := s_fixi;
	end;
	//Indentificando xo
	FUNCTION id_xo(classe : classes; xi : vetor_classe; q_classe : Integer) : Integer;
	var
		maior, id : Integer;
	begin
		maior := 0;
		id := 0;
		FOR i := 1 TO q_classe DO
		begin
			IF classe.fi[i] > maior THEN
			begin
				maior := classe.fi[i];
				id := i;
			end;
		end;
		id_xo := xi[id];
	end;
	//Calculando o h (amplitude)
	FUNCTION calc_h(classe : classes) : Integer;
	begin
		calc_h := classe.lsup[1] - classe.linf[1];
	end;
	//Calculando o yi
	FUNCTION calc_yi(xi : vetor_classe; xo : Integer; h : Integer; q_classe : Integer) : vetor_classe;
	begin
		FOR i := 1 TO q_classe DO
			yi[i] := (xi[i] - xo) div h;
		calc_yi := yi;
	end;
	//Calculando o fiyi
	FUNCTION calc_fiyi(classe : classes; yi : vetor_classe; q_classe : Integer) : vetor_classe;
	begin
		FOR i := 1 TO q_classe DO
			fiyi[i] := classe.fi[i] * yi[i];
		calc_fiyi := fiyi;
	end;
	//Somando o fiyi
	FUNCTION soma_fiyi(fiyi : vetor_classe; q_classe : Integer) : Integer;
	var
		s_fiyi : Integer;
	begin
		s_fiyi := 0;
		FOR i := 1 TO q_classe DO
			INC(s_fiyi, fiyi[i]);
		soma_fiyi := s_fiyi;
	end;
	//Calculando mo
	FUNCTION calc_mo(classe : classes; q_classe : Integer) : Integer;
	var
		maior, id : Integer;
	begin
		maior := 0;
		id := 0;
		FOR i := 1 TO q_classe DO
		begin
			IF classe.fi[i] > maior THEN
			begin
				maior := classe.fi[i];
				id := i;
			end;
		end;
		mo := (classe.linf[id] + classe.lsup[id]) div 2;
		calc_mo := mo;
	end;
	//Calculando Fi
	FUNCTION calc_Fzi(classe : classes; q_classe : Integer) : vetor_classe;
	begin
		FOR i := 1 TO q_classe DO
		begin
			IF i > 1 THEN
				Fzi[i] := Fzi[i - 1] + classe.fi[i]
			ELSE Fzi[i] := classe.fi[i];
		end;
		calc_Fzi := Fzi;
	end;
	//Indentificando Fi Ant
	FUNCTION id_Fzi_ant(classe : classes; Fzi : vetor_classe; q_classe : Integer) : Integer;
	var
		maior, id : Integer;
	begin
		maior := 0;
		id := 0;
		FOR i := 1 TO q_classe DO
		begin
			IF classe.fi[i] > maior THEN
			begin
				maior := classe.fi[i];
				id := i;
			end;
		end;
		id_Fzi_ant := Fzi[id - 1];
	end;
	//Indentificando limite inferior da classe mediana
	FUNCTION id_l(classe : classes; q_classe : Integer) : Integer;
	var
		maior, id : Integer;
	begin
		maior := 0;
		id := 0;
		FOR i := 1 TO q_classe DO
		begin
			IF classe.fi[i] > maior THEN
			begin
				maior := classe.fi[i];
				id := i;
			end;
		end;
		id_l := classe.linf[id];
	end;
	//Indentificando maior frequância simples da classe mediana
	FUNCTION id_f(classe : classes; q_classe : Integer) : Integer;
	var
		maior, id : Integer;
	begin
		maior := 0;
		id := 0;
		FOR i := 1 TO q_classe DO
		begin
			IF classe.fi[i] > maior THEN
			begin
				maior := classe.fi[i];
				id := i;
			end;
		end;
		id_f := classe.fi[id];
	end;
	//Calculando o xi²
	FUNCTION calc_xi2(xi : vetor_classe; q_classe : Integer) : vetor_classe;
	begin
		FOR i := 1 TO q_classe DO 
			xi2[i] := xi[i] * xi[i];
		calc_xi2 := xi2;
	end;
	//Calculando o fixi²
	FUNCTION calc_fixi2(classe : classes; xi2 : vetor_classe; q_classe : Integer) : vetor_classe;
	begin
		FOR i := 1 TO q_classe DO 
			fixi2[i] := classe.fi[i] * xi2[i];
		calc_fixi2 := fixi2;
	end;
	//Somando o fixi²
	FUNCTION soma_fixi2(fixi2 : vetor_classe; q_classe : Integer) : Longint;
	var
		s_fixi2 : Longint;
	begin
		s_fixi2 := 0;
		FOR i := 1 TO q_classe DO
			INC(s_fixi2, fixi2[i]);
		soma_fixi2 := s_fixi2;
	end;
	//Calculando o yi²
	FUNCTION calc_yi2(yi : vetor_classe; q_classe : Integer) : vetor_classe;
	begin
		FOR i := 1 TO q_classe DO 
			yi2[i] := yi[i] * yi[i];
		calc_yi2 := yi2;
	end;
	//Calculando o fiyi²
	FUNCTION calc_fiyi2(classe : classes; yi2 : vetor_classe; q_classe : Integer) : vetor_classe;
	begin
		FOR i := 1 TO q_classe DO 
			fiyi2[i] := classe.fi[i] * yi2[i];
		calc_fiyi2 := fiyi2;
	end;
	//Somando o fiyi²
	FUNCTION soma_fiyi2(fiyi2 : vetor_classe; q_classe : Integer) : Longint;
	var
		s_fiyi2 : Longint;
	begin
		s_fiyi2 := 0;
		FOR i := 1 TO q_classe DO
			INC(s_fiyi2, fiyi2[i]);
		soma_fiyi2 := s_fiyi2;
	end;
	//Calculando Termo Quartil
	FUNCTION calc_termo_q(k : Integer; s_fi : Integer) : Real;
	var
		t : Real;
	begin
		t := 0;
		t := (k * s_fi) / 4;
		calc_termo_q := t;
	end;
	//Indentificando classe do Quartil
	FUNCTION classe_q(termo_quartil : Real; Fzi : vetor_classe; classe : classes; q_classe : Integer) : quartil;
	Var
		calc_q : quartil;
	begin
		i := 0;
		REPEAT
			INC(i);
		UNTIL (termo_quartil <= Fzi[i]) OR (i > q_classe);
		
		IF i > q_classe THEN
			writeln('Termo do quartil maior que a quantidade de classes.')
		ELSE
		begin
			IF i = 1 THEN
				calc_q.Fzi_ant := 0
			ELSE	calc_q.Fzi_ant := Fzi[i - 1];
			calc_q.l := classe.linf[i];
			calc_q.f := classe.fi[i];
		end;
		classe_q := calc_q;
	end;
	//Calculando Termo Percentil
	FUNCTION calc_termo_p(k : Integer; s_fi : Integer) : Real;
	var
		t : Real;
	begin
		t := 0;
		t := (k * s_fi) / 100;
		calc_termo_p := t;
	end;
	//Indentificando classe do Percentil
	FUNCTION classe_p(termo_percentil : Real; Fzi : vetor_classe; classe : classes; q_classe : Integer) : quartil;
	Var
		calc_p : quartil;
	begin
		i := 0;
		REPEAT
			INC(i);
		UNTIL (termo_percentil <= Fzi[i]) OR (i > q_classe);
		
		IF i > q_classe THEN
			writeln('Termo do percentil maior que a quantidade de classes.')
		ELSE
		begin
			IF i = 1 THEN
				calc_p.Fzi_ant := 0
			ELSE	calc_p.Fzi_ant := Fzi[i - 1];
			calc_p.l := classe.linf[i];
			calc_p.f := classe.fi[i];
		end;
		classe_p := calc_p;
	end;
	
Begin
	clrscr;
	k := 0;
	writeln('	/-----------------------MENU-----------------------\');
	writeln('	|1 - Calcular a Média Processo Normal.             |');
	writeln('	|2 - Calcular a Média Processo Breve.              |');
	writeln('	|3 - Calcular a Médiana.                           |');
	writeln('	|4 - Calcular o Quartil.                           |');
	writeln('	|5 - Calcular o Percentil.                         |');
	writeln('	|6 - Calcular Desvio Padrão Processo Normal.       |');
	writeln('	|7 - Calcular Desvio Padrão Processo Breve.        |');
	writeln('	|8 - Coeficiente de Variação.                      |');
	writeln('	|9 - Coeficiente de Assimetria.                    |');
	writeln('	|10- Coeficiente de Curtose.                       |');
	writeln('	|0 - Sair.                                         |');
	writeln('	\--------------------------------------------------/');	
	writeln;
	writeln('Digite a opção: ');
	read(opc);
	clrscr;
	
	//INSERÇÃO DE VALORES
	writeln;
	writeln('Digite a quantidade de classes: ');
	read(q_classe);	
	writeln('Digite a amplitude: ');
	read(amp);
	
	FOR i := 1 TO q_classe DO//Inserção de Dados das classes
	begin
		IF i = 1 THEN
		begin
			writeln('Digite o Linf da ', i, 'ª classe: ');
			read(classe.linf[i]);
			classe.lsup[i] := classe.linf[i] + amp;
			clrscr;
			writeln('----------------fi----------------');
		end
		ELSE
		begin
			classe.linf[i] := classe.linf[i - 1] + amp;
			classe.lsup[i] := classe.lsup[i - 1] + amp;
		end;
		writeln('Digite o fi da ', i, 'ª classe: ');
		read(classe.fi[i]);
	end;
	clrscr;
	
	CASE opc OF
	1 : 	begin//Calcular a Média Processo Normal
			//Somatório fixi
			xi := calc_xi(classe, q_classe);
			fixi := calc_fixi(classe, xi, q_classe);
			s_fixi := soma_fixi(fixi, q_classe);
			//Somatório fi
			s_fi := soma_fi(classe, q_classe);
			//Resultado Média Processo Normal
			r_med_normal := s_fixi / s_fi;
			
			writeln('-----------------------------------------------------');
			writeln;
			writeln;
			writeln('	classes');
			FOR i := 1 TO q_classe DO
				writeln('	',classe.linf[i],'|--',classe.linf[i]);
				writeln;
			FOR i := 1 TO q_classe DO
				writeln('	fi = ', classe.linf[i]);
				writeln('	Somatório de fi = ', s_fi);
				writeln;
	
			FOR i := 1 TO q_classe DO
				writeln('	xi = ', xi[i]);
				writeln;
			FOR i := 1 TO q_classe DO
				writeln('	fixi = ', fixi[i]);
			writeln('	Somatório de fixi = ', s_fixi);
			
			writeln;
			writeln('	Média processo normal x = ', r_med_normal:0:3);
			writeln;
			writeln;
			writeln;
			writeln('-----------------------------------------------------');
			readln;
		end;
	2 : 	begin//Calcular a Média Processo Breve
			//xo
			xi := calc_xi(classe, q_classe);
			xo := id_xo(classe, xi, q_classe);
			//Amplitude
			h := calc_h(classe);
			//Somatório fiyi
			yi := calc_yi(xi, xo, h, q_classe);
			fiyi := calc_fiyi(classe, yi, q_classe);
			s_fiyi := soma_fiyi(fiyi, q_classe);
			//Somatório fi
			s_fi := soma_fi(classe, q_classe);
			//Resultado Média Processo Brve
			r_med_breve := xo + ((s_fiyi * h) / s_fi);
			
			writeln('-----------------------------------------------------');
			writeln;
			writeln;
			writeln('	classes');
			FOR i := 1 TO q_classe DO
				writeln('	',classe.linf[i],'|--',classe.linf[i]);
				writeln;
			FOR i := 1 TO q_classe DO
				writeln('	fi = ', classe.linf[i]);
				writeln('	Somatório de fi = ', s_fi);
				writeln;
	
			FOR i := 1 TO q_classe DO
				writeln('	yi = ', yi[i]);
				writeln;
			FOR i := 1 TO q_classe DO
				writeln('	fiyi = ', fiyi[i]);
			writeln('	Somatório de fiyi = ', s_fiyi);
			
			writeln;
			writeln('	xo = ', xo);
			writeln('	h = ', h);
			writeln;
			writeln('	Média processo breve x = ', r_med_breve:0:3);
			writeln;
			writeln;
			writeln;
			writeln('-----------------------------------------------------');
			readln;
		end;
	3 : 	begin//Calcular a Médiana
			//l*
			l := id_l(classe, q_classe);
			//Somatório fi
			s_fi := soma_fi(classe, q_classe);
			//Fi Anterior
			Fzi := calc_Fzi(classe, q_classe);
			Fzi_ant := id_Fzi_ant(classe, Fzi, q_classe);
			//Amplitude
			h := calc_h(classe);
			//f*
			f := id_f(classe, q_classe);
			//Resultado Médiana
			md := l + ((((s_fi / 2) - Fzi_ant) * h) / f);
			
			writeln('-----------------------------------------------------');
			writeln;
			writeln;
			writeln('	classes');
			FOR i := 1 TO q_classe DO
				writeln('	',classe.linf[i],'|--',classe.linf[i]);
				writeln;
			FOR i := 1 TO q_classe DO
				writeln('	fi = ', classe.linf[i]);
				writeln('	Somatório de fi = ', s_fi);
				writeln;
	
			FOR i := 1 TO q_classe DO
				writeln('	Fi = ', Fzi[i]);
				writeln;
			
			writeln;
			writeln('	l* = ', l);
			writeln('	Fi Ant = ', Fzi_ant);
			writeln('	h = ', h);
			writeln('	f* = ', f);
			writeln;
			writeln('	Mediana = ', md:0:3);
			writeln;
			writeln;
			writeln;
			writeln('-----------------------------------------------------');
			readln;
		end;
	4 : 	begin//Calcular o Quartil
			writeln('Digite o Quartil que deseja calcular: ');
			read(k);
			clrscr;
			//Termo
			s_fi := soma_fi(classe, q_classe);
			termo_quartil := calc_termo_q(k, s_fi);
			//Classe Quartil : l*, Fi anterior, f*
			Fzi := calc_Fzi(classe, q_classe);
			qk := classe_q(termo_quartil, Fzi, classe, q_classe);
			//Amplitude
			h := calc_h(classe);
			//Resultado Quartil
			r_q := qk.l + (((termo_quartil - qk.Fzi_ant) * h) / qk.f);

			writeln('-----------------------------------------------------');
			writeln;
			writeln;
			writeln('	classes');
			FOR i := 1 TO q_classe DO
				writeln('	',classe.linf[i],'|--',classe.linf[i]);
				writeln;
			FOR i := 1 TO q_classe DO
				writeln('	fi = ', classe.linf[i]);
				writeln('	Somatório de fi = ', s_fi);
				writeln;
	
			FOR i := 1 TO q_classe DO
				writeln('	Fi = ', Fzi[i]);
				writeln;
			
			writeln;
			writeln('	Termo = ', termo_quartil:0:3);
			writeln('	l* = ', qk.l);
			writeln('	Fi anterior = ', qk.Fzi_ant);
			writeln('	h = ', h);
			writeln('	f* = ', qk.f);
			writeln;
			writeln('	 ', k, 'ª Quartil  = ', r_q:0:3);
			writeln;
			writeln;
			writeln;
			writeln('-----------------------------------------------------');
			readln;
		end;
	5 : 	begin//Calcular o Percentil
			writeln('Digite o Percentil que deseja calcular: ');
			read(k);
			clrscr;
			//Termo
			s_fi := soma_fi(classe, q_classe);
			termo_percentil := calc_termo_p(k, s_fi);
			//Classe Quartil : l*, Fi anterior, f*
			Fzi := calc_Fzi(classe, q_classe);
			pk := classe_p(termo_percentil, Fzi, classe, q_classe);
			//Amplitude
			h := calc_h(classe);
			//Resultado Quartil
			r_p := pk.l + (((termo_percentil - pk.Fzi_ant) * h) / pk.f);
			
			writeln('-----------------------------------------------------');
			writeln;
			writeln;
			writeln('	classes');
			FOR i := 1 TO q_classe DO
				writeln('	',classe.linf[i],'|--',classe.linf[i]);
				writeln;
			FOR i := 1 TO q_classe DO
				writeln('	fi = ', classe.linf[i]);
				writeln('	Somatório de fi = ', s_fi);
				writeln;
	
			FOR i := 1 TO q_classe DO
				writeln('	Fi = ', Fzi[i]);
				writeln;
				
			writeln('	Somatório de fiyi = ', s_fiyi);
			writeln;
			writeln('	termo = ', termo_percentil:0:3);
			writeln('	l* = ', pk.l);
			writeln('	Fi anterior = ', pk.Fzi_ant);
			writeln('	h = ', h);
			writeln('	f* = ', pk.f);
			writeln;
			writeln('	 ', k, 'ª Percentil  = ', r_p:0:3);
			writeln;
			writeln;
			writeln;
			writeln('-----------------------------------------------------');
			readln;
		end;
	6 : 	begin//Calcular Desvio Padrão Processo Normal
			//Somatório fi
			s_fi := soma_fi(classe, q_classe);
			//Somatório fixi
			xi := calc_xi(classe, q_classe);
			fixi := calc_fixi(classe, xi, q_classe);
			s_fixi := soma_fixi(fixi, q_classe);
			//Somatório fixi2
			xi2 := calc_xi2(xi, q_classe);
			fixi2 := calc_fixi2(classe, xi2, q_classe);
			s_fixi2 := soma_fixi2(fixi2, q_classe);
			//n
			n := soma_fi(classe, q_classe);
			//Resultado Desvio Padrão Processo Normal
			s := sqrt((s_fixi2 / n) - EXP(ln(s_fixi / n) * 2));
			
			writeln('-----------------------------------------------------');
			writeln;
			writeln;
			writeln('	classes');
			FOR i := 1 TO q_classe DO
				writeln('	',classe.linf[i],'|--',classe.linf[i]);
				writeln;
			FOR i := 1 TO q_classe DO
				writeln('	fi = ', classe.linf[i]);
				writeln('	Somatório de fi = ', s_fi);
				writeln;
	
			FOR i := 1 TO q_classe DO
				writeln('	xi = ', xi[i]);
				writeln;
			FOR i := 1 TO q_classe DO
				writeln('	fixi = ', fixi[i]);
			writeln('	Somatório de fixi = ', s_fixi);
			writeln;
			FOR i := 1 TO q_classe DO
				writeln('	xi2 = ', xi2[i]);
				writeln;
			FOR i := 1 TO q_classe DO
				writeln('	fixi2 = ', fixi2[i]);
			writeln('	Somatório de fixi2 = ', s_fixi2:0:3);
			
			writeln;
			writeln('	n = ', n);
			writeln;
			writeln('	Desvio Padrão Processo Normal s= ', s:0:3);
			writeln;
			writeln;
			writeln;
			writeln('-----------------------------------------------------');
			readln;
		end;
	7 : 	begin//Calcular Desvio Padrão Processo Breve
			//Somatório fi
			s_fi := soma_fi(classe, q_classe);
			//xo
			xi := calc_xi(classe, q_classe);
			xo := id_xo(classe, xi, q_classe);
			//Amplitude
			h := calc_h(classe);
			//Somatório fiyi
			yi := calc_yi(xi, xo, h, q_classe);
			fiyi := calc_fiyi(classe, yi, q_classe);
			s_fiyi := soma_fiyi(fiyi, q_classe);
			//Somatório fiyi2
			yi2 := calc_yi2(yi, q_classe);
			fiyi2 := calc_fiyi2(classe, yi2, q_classe);
			s_fiyi2 := soma_fiyi2(fiyi2, q_classe);	
			//n
			n := soma_fi(classe, q_classe);
			//Resultado Desvio Padrão Processo Breve
			s := h * (sqrt((s_fiyi2 / n) - EXP(ln(s_fiyi / n) * 2)));
			
			writeln('-----------------------------------------------------');
			writeln;
			writeln;
			writeln('	classes');
			FOR i := 1 TO q_classe DO
				writeln('	',classe.linf[i],'|--',classe.linf[i]);
				writeln;
			FOR i := 1 TO q_classe DO
				writeln('	fi = ', classe.linf[i]);
				writeln('	Somatório de fi = ', s_fi);
				writeln;
	
			FOR i := 1 TO q_classe DO
				writeln('	yi = ', yi[i]);
				writeln;
			FOR i := 1 TO q_classe DO
				writeln('	fiyi = ', fiyi[i]);
			writeln('	Somatório de fiyi = ', s_fiyi);
			writeln;
			FOR i := 1 TO q_classe DO
				writeln('	yi2 = ', yi2[i]);
				writeln;
			FOR i := 1 TO q_classe DO
				writeln('	fiyi2 = ', fiyi2[i]);
			writeln('	Somatório de fiyi2 = ', s_fiyi2:0:3);
			
			writeln;
			writeln('	xo = ', xo);
			writeln('	h = ', h);
			writeln('	n = ', n);
			writeln;
			writeln('	Desvio Padrão Processo Breve s= ', s:0:3);
			writeln;
			writeln;
			writeln;
			writeln('-----------------------------------------------------');
			readln;
		end;
	8 : 	begin//Coeficiente de Variação
			//Média processo normal
			//Somatório fixi
			xi := calc_xi(classe, q_classe);
			fixi := calc_fixi(classe, xi, q_classe);
			s_fixi := soma_fixi(fixi, q_classe);
			//Somatório fi
			s_fi := soma_fi(classe, q_classe);
			//Resultado Média Processo Normal
			r_med_normal := s_fixi / s_fi;
			
			//Desvio Padrão Processo Breve
			//Somatório fi
			s_fi := soma_fi(classe, q_classe);
			//xo
			xi := calc_xi(classe, q_classe);
			xo := id_xo(classe, xi, q_classe);
			//Amplitude
			h := calc_h(classe);
			//Somatório fiyi
			yi := calc_yi(xi, xo, h, q_classe);
			fiyi := calc_fiyi(classe, yi, q_classe);
			s_fiyi := ABS(soma_fiyi(fiyi, q_classe));
			//Somatório fiyi2
			yi2 := calc_yi2(yi, q_classe);
			fiyi2 := calc_fiyi2(classe, yi2, q_classe);
			s_fiyi2 := soma_fiyi2(fiyi2, q_classe);	
			//n
			n := soma_fi(classe, q_classe);
			//Resultado Desvio Padrão Processo Breve
			s := h * (sqrt((s_fiyi2 / n) - EXP(ln(s_fiyi / n) * 2)));

			cv := (s / r_med_normal) * 100;
			
			writeln('-----------------------------------------------------');
			writeln;
			writeln;
			writeln('	classes');
			FOR i := 1 TO q_classe DO
				writeln('	',classe.linf[i],'|--',classe.linf[i]);
				writeln;
			FOR i := 1 TO q_classe DO
				writeln('	fi = ', classe.linf[i]);
				writeln('	Somatório de fi = ', s_fi);
				writeln;
	
			FOR i := 1 TO q_classe DO
				writeln('	xi = ', xi[i]);
				writeln;
			FOR i := 1 TO q_classe DO
				writeln('	fixi = ', fixi[i]);
			writeln('	Somatório de fixi = ', s_fixi);
			writeln;
			FOR i := 1 TO q_classe DO
				writeln('	yi = ', yi[i]);
			writeln;
			FOR i := 1 TO q_classe DO
				writeln('	fiyi = ', fiyi[i]);
			writeln('	Somatório de fiyi = ', s_fiyi);
			writeln;
			FOR i := 1 TO q_classe DO
				writeln('	yi2 = ', yi2[i]);
				writeln;
			FOR i := 1 TO q_classe DO
				writeln('	fiyi2 = ', fiyi2[i]);
			writeln('	Somatório de fiyi2 = ', s_fiyi2:0:3);
			
			writeln;
			writeln('	Média processo normal x = ', r_med_normal:0:3);
			writeln;
			writeln('	xo = ', xo);
			writeln('	h = ', h);
			writeln('	n = ', n);
			writeln('	Desvio Padrão Processo Breve s= ', s:0:3);
			writeln;
			writeln('	Coeficiente de variação CV= ', cv:0:3, '%');
			writeln;
			writeln;
			writeln;
			writeln('-----------------------------------------------------');
			readln;
		end;
	9 : 	begin//Coeficiente de Assimetria
			//Média processo normal
			//Somatório fixi
			xi := calc_xi(classe, q_classe);
			fixi := calc_fixi(classe, xi, q_classe);
			s_fixi := soma_fixi(fixi, q_classe);
			//Somatório fi
			s_fi := soma_fi(classe, q_classe);
			//Resultado Média Processo Normal
			r_med_normal := s_fixi / s_fi;
	
			//Médiana
			//l*
			l := id_l(classe, q_classe);
			//Fi Anterior
			Fzi := calc_Fzi(classe, q_classe);
			Fzi_ant := id_Fzi_ant(classe, Fzi, q_classe);
			//Amplitude
			h := calc_h(classe);
			//f*
			f := id_f(classe, q_classe);
			//Resultado Médiana
			md := l + ((((s_fi / 2) - Fzi_ant) * h) / f);
			
			//Desvio Padrão
			//xo
			xi := calc_xi(classe, q_classe);
			xo := id_xo(classe, xi, q_classe);
			//Somatório fiyi
			yi := calc_yi(xi, xo, h, q_classe);
			fiyi := calc_fiyi(classe, yi, q_classe);
			s_fiyi := ABS(soma_fiyi(fiyi, q_classe));
			//Somatório fiyi2
			yi2 := calc_yi2(yi, q_classe);
			fiyi2 := calc_fiyi2(classe, yi2, q_classe);
			s_fiyi2 := soma_fiyi2(fiyi2, q_classe);	
			//n
			n := soma_fi(classe, q_classe);
			//Resultado Desvio Padrão Processo Breve
			s := h * (sqrt((s_fiyi2 / n) - EXP(ln(s_fiyi / n) * 2)));
			
			r_as := (3 * (r_med_normal - md)) / s;
			
			writeln('-----------------------------------------------------');
			writeln;
			writeln;
			writeln('	classes');
			FOR i := 1 TO q_classe DO
				writeln('	',classe.linf[i],'|--',classe.linf[i]);
				writeln;
			FOR i := 1 TO q_classe DO
				writeln('	fi = ', classe.linf[i]);
				writeln('	Somatório de fi = ', s_fi);
				writeln;
			
			FOR i := 1 TO q_classe DO
				writeln('	Fi = ', Fzi[i]);
			writeln;
			FOR i := 1 TO q_classe DO
				writeln('	xi = ', xi[i]);
			writeln;
			FOR i := 1 TO q_classe DO
				writeln('	fixi = ', fixi[i]);
			writeln('	Somatório de fixi = ', s_fixi);
			writeln;
			FOR i := 1 TO q_classe DO
				writeln('	yi = ', yi[i]);
			writeln;
			FOR i := 1 TO q_classe DO
				writeln('	fiyi = ', fiyi[i]);
			writeln('	Somatório de fiyi = ', s_fiyi);
			writeln;
			FOR i := 1 TO q_classe DO
				writeln('	yi2 = ', yi2[i]);
			writeln;
			FOR i := 1 TO q_classe DO
				writeln('	fiyi2 = ', fiyi2[i]);
			writeln('	Somatório de fiyi2 = ', s_fiyi2:0:3);
			
			writeln;
			writeln('	Média processo normal x = ', r_med_normal:0:3);
			writeln;
			writeln('	l* = ', l);
			writeln('	sFi Ant = ', Fzi_ant);
			writeln('	h = ', h);
			writeln('	f* = ', f);
			writeln('	Mediana = ', md:0:3);
			writeln;
			writeln('	xo = ', xo);
			writeln('	n = ', n);
			writeln('	Desvio Padrão Processo Breve s= ', s:0:3);
			writeln;
			writeln('	Coeficiente de Assimetria AS= ', r_as:0:3);
			IF r_as = 0 THEN
				writeln('	AS = 0 -> Distribuição é simétrica.')
			ELSE IF r_as > 0 THEN writeln('	AS > 0 -> distribuição é assimétrica positiva ou à direita.')
			ELSE writeln('	AS < 0 -> distribuição é assimétrica negativa ou à esquerda.');
			writeln;
			writeln;
			writeln;
			writeln('-----------------------------------------------------');
			readln;									
		end;
	10 : 	begin//Coeficiente de Curtose
			//Quartil 3
			k := 3;
			//Termo
			s_fi := soma_fi(classe, q_classe);
			termo_quartil := calc_termo_q(k, s_fi);
			
			//Classe Quartil : l*, Fi anterior, f*
			Fzi := calc_Fzi(classe, q_classe);
			qk := classe_q(termo_quartil, Fzi, classe, q_classe);
			writeln('-----------------------------------------------------');
			writeln;
			writeln;
			writeln('	3ª Quartil');
			writeln('	Termo Q3 = ', termo_quartil:0:3);
			writeln('	l* = ', qk.l);
			writeln('	Fi anterior = ', qk.Fzi_ant);
			writeln('	f* = ', qk.f);
			//Amplitude
			h := calc_h(classe);
			//Resultado Quartil
			q3 := qk.l + (((termo_quartil - qk.Fzi_ant) * h) / qk.f);
			writeln;
						
			//Quartil 1
			k := 1;
			//Termo
			s_fi := soma_fi(classe, q_classe);
			termo_quartil := calc_termo_q(k, s_fi);
			//Classe Quartil : l*, Fi anterior, f*
			Fzi := calc_Fzi(classe, q_classe);
			qk := classe_q(termo_quartil, Fzi, classe, q_classe);
			writeln('	1ª Quartil');
			writeln('	Termo Q1 = ', termo_quartil:0:3);
			writeln('	l* = ', qk.l);
			writeln('	Fi anterior = ', qk.Fzi_ant);
			writeln('	f* = ', qk.f);
			//Amplitude
			h := calc_h(classe);
			//Resultado Quartil
			q1 := qk.l + (((termo_quartil - qk.Fzi_ant) * h) / qk.f);
			writeln;
			
			//Percentil 90
			k := 90;
			//Termo
			s_fi := soma_fi(classe, q_classe);
			termo_percentil := calc_termo_p(k, s_fi);
			//Classe Quartil : l*, Fi anterior, f*
			Fzi := calc_Fzi(classe, q_classe);
			pk := classe_p(termo_percentil, Fzi, classe, q_classe);
			writeln('	Percentil 90');
			writeln('	Termo P90 = ', termo_percentil:0:3);
			writeln('	l* = ', pk.l);
			writeln('	Fi anterior = ', pk.Fzi_ant);
			writeln('	f* = ', pk.f);
			//Amplitude
			h := calc_h(classe);
			//Resultado Quartil
			p90 := pk.l + (((termo_percentil - pk.Fzi_ant) * h) / pk.f);
			writeln;
			
			//Percentil 10
			k := 10;
			//Termo
			s_fi := soma_fi(classe, q_classe);
			termo_percentil := calc_termo_p(k, s_fi);
			//Classe Quartil : l*, Fi anterior, f*
			Fzi := calc_Fzi(classe, q_classe);
			pk := classe_p(termo_percentil, Fzi, classe, q_classe);
			writeln('	Percentil 10');
			writeln('	Termo P10 = ', termo_percentil:0:3);
			writeln('	l* = ', pk.l);
			writeln('	Fi anterior = ', pk.Fzi_ant);
			writeln('	f* = ', pk.f);
			//Amplitude
			h := calc_h(classe);
			//Resultado Quartil
			p10 := pk.l + (((termo_percentil - pk.Fzi_ant) * h) / pk.f);
			writeln;
			
			c := (q3 - q1) / (2 * (p90 - p10));
			
			writeln;
			writeln('	classes');
			FOR i := 1 TO q_classe DO
				writeln('	',classe.linf[i],'|--',classe.linf[i]);
				writeln;
			FOR i := 1 TO q_classe DO
				writeln('	fi = ', classe.linf[i]);
				writeln('	Somatório de fi = ', s_fi);
				writeln;
			
			FOR i := 1 TO q_classe DO
				writeln('	Fi = ', Fzi[i]);
			writeln;
			
			writeln;
			writeln('h = ', h);
			writeln;
			writeln('	3ª Quartil = ', q3:0:3);
			:qwriteln('	1ª Quartil = ', q1:0:3);
			writeln;
			writeln('	Percentil 90 = ', p90:0:3);
			writeln('	Percentil 10 = ', p10:0:3);
			writeln;
			writeln('	Curtose = ', c:0:3);
			writeln;
			IF c = 0.263 THEN
				writeln('	c = 0,263 -> Distribuição Mesocúrtica.')
			ELSE IF c > 0.263 THEN writeln('	c > 0,263 -> Distribuição Platicúrtica.')
			ELSE writeln('	c < 0,263 -> Distribuição Leptocúrtica.');
			writeln;
			writeln;
			writeln;
			writeln('-----------------------------------------------------');
			readln;		
		end;
	ELSE	begin
			writeln('Opção inválida.');
			writeln;
			readln;
		end;
	END;

	//CALCULOS
	{
	mo := calc_mo(classe, q_classe);
	writeln('mo = ', mo);
	}
End.
