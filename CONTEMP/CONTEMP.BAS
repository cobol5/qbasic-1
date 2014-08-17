DECLARE SUB menurelatorios ()
DECLARE SUB removerclientes ()
DECLARE SUB removerfuncionarios ()
DECLARE SUB removerprodutos ()
DECLARE SUB removerfornecedores ()
DECLARE SUB cadastroclientes ()
DECLARE SUB cadastrovendedores ()
DECLARE SUB cadastrofuncionarios ()
DECLARE SUB barra (mensagem$)
DECLARE SUB cadastroprodutos ()
DECLARE SUB Ended ()
DECLARE SUB passwords ()
DECLARE SUB cadastrofornecedores ()
DECLARE SUB menuprogramas ()
DECLARE SUB menuopcoes ()
DECLARE SUB menuvendas ()
DECLARE SUB menulocalizar ()
DECLARE SUB menuremocao ()
DECLARE SUB menucadastro ()
DECLARE SUB msgbox (entrada$, a)
DECLARE SUB menuprincipal ()
DECLARE SUB principal ()
CONST temp$ = "c:\temp.tmp"
CLS
CHDRIVE "c"
CHDIR "C:\ContEmp"
passwords

SUB barra (mensagem$)
mensagem$ = UCASE$(mensagem$)
LOCATE 22, 3
SELECT CASE mensagem$
        CASE "NENCONTRADO"
                PRINT "O cขdigo digitado nฦo foi encontrado!"
                r$ = INPUT$(1)
        CASE "CODIGOVAZIO"
                PRINT "Para voltar a tela anterior, deixe o codigo vazio."
        CASE "LIMPAR"
                PRINT "                                                                           "
        CASE "EXISTE"
                PRINT "Este cขdigo j  est  cadastrado!"
        CASE "NAOEXISTE"
                PRINT "Cadastro nฦo encontrado!"
        CASE "PAUSE"
                PRINT "Pressione qualquer tecla para continuar..."
                r$ = INPUT$(1)
        CASE "ERRORESC"
                PRINT "Erro na escolha, tente novamente!"
                r$ = INPUT$(1)
        END SELECT
END SUB

SUB cadastroclientes
FOR a = 4 TO 20
LOCATE a, 1
PRINT "บ                                                                             บ"

NEXT a
COLOR 15, 4: LOCATE 2, 3: PRINT "InfoBrasil บ ContEmp 1.0 - Cadastro de Clientes                บ "; DATE$
COLOR 15, 1

FOR a = 4 TO 16 STEP 2
LOCATE a, 3
SELECT CASE a
        CASE 4
        PRINT "Cขdigo:"
        CASE 6
        PRINT "Nome:"
        CASE 8
        PRINT "Endereo:"
        CASE 10
        PRINT "Bairro:                  Cidade:                      Estado:"
        CASE 12
        PRINT "CEP:             C.P.F.:                   Telefone:"
        CASE 14
        PRINT "E-mail:"
        CASE 16
        PRINT "Observaไes:"
        CASE ELSE
        END SELECT
NEXT a
barra "LIMPAR"

FOR a = 4 TO 16
SELECT CASE a
        CASE 4
        barra "CODIGOVAZIO"
6 LOCATE a, 11
        LINE INPUT "", codigo$
        IF codigo$ = "" THEN principal
        CHDIR "cli"
        SHELL "dir/on/b *.* > c:\temp.tmp"
        OPEN temp$ FOR INPUT AS #1
        WHILE NOT EOF(1)
        LINE INPUT #1, a$
        IF UCASE$(a$) = UCASE$(codigo$) THEN
                barra "EXISTE"
                CLOSE #1
                CHDIR ".."
                GOTO 6
                END IF
        WEND
        CLOSE #1
        CHDIR ".."
        KILL temp$
        CASE 6
        LOCATE a, 9
        LINE INPUT "", nome$
        CASE 8
        LOCATE a, 13
        LINE INPUT "", endereco$
        CASE 10
        LOCATE a, 11
        LINE INPUT "", bairro$
        LOCATE a, 36
        LINE INPUT "", cidade$
        LOCATE a, 65
        LINE INPUT "", estado$
        CASE 12
        LOCATE a, 8
        LINE INPUT "", cep$
        LOCATE a, 28
        LINE INPUT "", cpf$
        LOCATE a, 56
        LINE INPUT "", telefone$
        CASE 14
        LOCATE a, 11
        LINE INPUT "", email$
        CASE 16
        LOCATE a, 15
        LINE INPUT "", obs$
        CASE ELSE
        END SELECT
NEXT a
CHDIR "cli"
OPEN codigo$ FOR OUTPUT AS #1
PRINT #1, nome$
PRINT #1, endereco$
PRINT #1, bairro$
PRINT #1, cidade$
PRINT #1, estado$
PRINT #1, cep$
PRINT #1, cpf$
PRINT #1, telefone$
PRINT #1, email$
PRINT #1, obs$
CLOSE #1
CHDIR ".."
msgbox "MAIS_UM", 0
END SUB

SUB cadastrofornecedores
FOR a = 4 TO 20
LOCATE a, 1
PRINT "บ                                                                             บ"
       
NEXT a
COLOR 15, 4: LOCATE 2, 3: PRINT "InfoBrasil บ ContEmp 1.0 - Cadastro de Fornecedores            บ "; DATE$
COLOR 15, 1

FOR a = 4 TO 16 STEP 2
LOCATE a, 3
SELECT CASE a
        CASE 4
        PRINT "Cขdigo:"
        CASE 6
        PRINT "Nome:"
        CASE 8
        PRINT "Endereo:"
        CASE 10
        PRINT "Bairro:                  Cidade:                      Estado:"
        CASE 12
        PRINT "CEP:             C.G.C.:                   Telefone:"
        CASE 14
        PRINT "E-mail:"
        CASE 16
        PRINT "Observaไes:"
        CASE ELSE
        END SELECT
NEXT a
barra "LIMPAR"

FOR a = 4 TO 16
SELECT CASE a
        CASE 4
        barra "CODIGOVAZIO"
3 LOCATE a, 11
        LINE INPUT "", codigo$
        IF codigo$ = "" THEN principal
        CHDIR "for"
        SHELL "dir/on/b *.* > c:\temp.tmp"
        OPEN temp$ FOR INPUT AS #1
        WHILE NOT EOF(1)
        LINE INPUT #1, a$
        IF UCASE$(a$) = UCASE$(codigo$) THEN
                barra "EXISTE"
                CLOSE #1
                CHDIR ".."
                GOTO 3
                END IF
        WEND
        CLOSE #1
        CHDIR ".."
        KILL temp$
        CASE 6
        LOCATE a, 9
        LINE INPUT "", nome$
        CASE 8
        LOCATE a, 13
        LINE INPUT "", endereco$
        CASE 10
        LOCATE a, 11
        LINE INPUT "", bairro$
        LOCATE a, 36
        LINE INPUT "", cidade$
        LOCATE a, 65
        LINE INPUT "", estado$
        CASE 12
        LOCATE a, 8
        LINE INPUT "", cep$
        LOCATE a, 28
        LINE INPUT "", cpf$
        LOCATE a, 56
        LINE INPUT "", telefone$
        CASE 14
        LOCATE a, 11
        LINE INPUT "", email$
        CASE 16
        LOCATE a, 15
        LINE INPUT "", obs$
        CASE ELSE
        END SELECT
NEXT a
CHDIR "For"
OPEN codigo$ FOR OUTPUT AS #1
PRINT #1, nome$
PRINT #1, endereco$
PRINT #1, bairro$
PRINT #1, cidade$
PRINT #1, estado$
PRINT #1, cep$
PRINT #1, cpf$
PRINT #1, telefone$
PRINT #1, email$
PRINT #1, obs$
CLOSE #1
CHDIR ".."
msgbox "MAIS_UM", 0
END SUB

SUB cadastrofuncionarios
FOR a = 4 TO 20
LOCATE a, 1
PRINT "บ                                                                             บ"

NEXT a
COLOR 15, 4: LOCATE 2, 3: PRINT "InfoBrasil บ ContEmp 1.0 - Cadastro de Funcion rios            บ "; DATE$
COLOR 15, 1

FOR a = 4 TO 16 STEP 2
LOCATE a, 3
SELECT CASE a
        CASE 4
        PRINT "Cขdigo:"
        CASE 6
        PRINT "Nome:"
        CASE 8
        PRINT "Endereo:"
        CASE 10
        PRINT "Bairro:                  Cidade:                      Estado:"
        CASE 12
        PRINT "CEP:             C.P.F.:                   Telefone:"
        CASE 14
        PRINT "E-mail:"
        CASE 16
        PRINT "Observaไes:"
        CASE ELSE
        END SELECT
NEXT a
barra "LIMPAR"

FOR a = 4 TO 16
SELECT CASE a
        CASE 4
        barra "CODIGOVAZIO"
4 LOCATE a, 11
        LINE INPUT "", codigo$
        IF codigo$ = "" THEN principal
        CHDIR "fun"
        SHELL "dir/on/b *.* > c:\temp.tmp"
        OPEN temp$ FOR INPUT AS #1
        WHILE NOT EOF(1)
        LINE INPUT #1, a$
        IF UCASE$(a$) = UCASE$(codigo$) THEN
                barra "EXISTE"
                CLOSE #1
                CHDIR ".."
                GOTO 4
                END IF
        WEND
        CLOSE #1
        CHDIR ".."
        KILL temp$
        CASE 6
        LOCATE a, 9
        LINE INPUT "", nome$
        CASE 8
        LOCATE a, 13
        LINE INPUT "", endereco$
        CASE 10
        LOCATE a, 11
        LINE INPUT "", bairro$
        LOCATE a, 36
        LINE INPUT "", cidade$
        LOCATE a, 65
        LINE INPUT "", estado$
        CASE 12
        LOCATE a, 8
        LINE INPUT "", cep$
        LOCATE a, 28
        LINE INPUT "", cpf$
        LOCATE a, 56
        LINE INPUT "", telefone$
        CASE 14
        LOCATE a, 11
        LINE INPUT "", email$
        CASE 16
        LOCATE a, 15
        LINE INPUT "", obs$
        CASE ELSE
        END SELECT
NEXT a
CHDIR "Fun"
OPEN codigo$ FOR OUTPUT AS #1
PRINT #1, nome$
PRINT #1, endereco$
PRINT #1, bairro$
PRINT #1, cidade$
PRINT #1, estado$
PRINT #1, cep$
PRINT #1, cpf$
PRINT #1, telefone$
PRINT #1, email$
PRINT #1, obs$
PRINT #1, "NAO"
CLOSE #1
CHDIR ".."
msgbox "MAIS_UM", 0

END SUB

SUB cadastroprodutos
FOR a = 4 TO 20
LOCATE a, 1
PRINT "บ                                                                             บ"

NEXT a
COLOR 15, 4: LOCATE 2, 3: PRINT "InfoBrasil บ ContEmp 1.0 - Cadastro de Produtos                บ "; DATE$
COLOR 15, 1

FOR a = 4 TO 14 STEP 2
LOCATE a, 3
SELECT CASE a
        CASE 4
        PRINT "Cขdigo:"
        CASE 6
        PRINT "Descriฦo:"
        CASE 8
        PRINT "Medida:"
        CASE 10
        PRINT "Quantidade:      Custo:            Venda:          Lucro:"
        CASE 12
        PRINT "Total de Custo:             Total de Venda:"
        CASE 14
        PRINT "Total de Lucro:"
        CASE ELSE
        END SELECT
NEXT a
barra "LIMPAR"
FOR a = 4 TO 14
SELECT CASE a
        CASE 4
        barra "CODIGOVAZIO"
2 LOCATE a, 11
        LINE INPUT "", codigo$
        IF codigo$ = "" THEN principal
        CHDIR "pro"
        SHELL "dir/on/b *.* > c:\temp.tmp"
        OPEN temp$ FOR INPUT AS #1
        WHILE NOT EOF(1)
        LINE INPUT #1, a$
        IF UCASE$(a$) = UCASE$(codigo$) THEN
                barra "EXISTE"
                CLOSE #1
                CHDIR ".."
                GOTO 2
                END IF
        WEND
        CLOSE #1
        CHDIR ".."
        KILL temp$
        CASE 6
        LOCATE a, 14
        LINE INPUT "", descricao$
        CASE 8
        LOCATE a, 11
        LINE INPUT "", medida$
        CASE 10
        LOCATE a, 15
        INPUT "", quantidade
        LOCATE a, 27
        INPUT "", custo
        LOCATE a, 45
        INPUT "", venda
        LOCATE a, 61
        lucro = venda - custo
        PRINT lucro
        CASE 12
        LOCATE a, 19
        totalcusto = quantidade * custo
        PRINT totalcusto
        LOCATE a, 47
        totalvenda = quantidade * venda
        PRINT totalvenda
        CASE 14
        LOCATE a, 17
        totallucro = totalvenda - totalcusto
        PRINT totallucro
        CASE ELSE
        END SELECT
NEXT a
CHDIR "pro"
OPEN codigo$ FOR OUTPUT AS #1
PRINT #1, descricao$
PRINT #1, medida$
PRINT #1, quantidade
PRINT #1, custo
PRINT #1, venda
PRINT #1, lucro
PRINT #1, totalcusto
PRINT #1, totalvenda
PRINT #1, totallucro
CLOSE #1
CHDIR ".."
msgbox "MAIS_UM", 1

END SUB

SUB cadastrovendedores
CLS
FOR a = 1 TO 23
LOCATE a, 1
SELECT CASE a
        CASE 1
        PRINT "ษออออออออออออหอออออออออออออออออออออออออออออออออออออออออออออออออออหออออออออออออป"
        CASE 2
        PRINT "บ            บ                                                   บ            บ"
        CASE 3
                PRINT "ฬออออออออออออฮอออออออออออออออออออออออออออออออออออออออออออออออออออฮออออออออออออน"
        CASE 5
                PRINT "ฬออออออออออออฮอออออออออออออออออออออออออออออออออออออออออออออออออออฮออออออออออออน"
        CASE 21
                PRINT "ฬออออออออออออสอออออออออออออออออออออออออออออออออออออออออออออออออออสออออออออออออน"
        CASE 22
                PRINT "บ                                                                             บ"
        CASE 23
                PRINT "ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ"
        CASE ELSE
                PRINT "บ            บ                                                   บ            บ"
        END SELECT
NEXT a
COLOR 15, 4: LOCATE 2, 3: PRINT "InfoBrasil บ ContEmp 1.0 - Cadastro de Vendedores              บ "; DATE$
COLOR 15, 1
LOCATE 4, 3:    PRINT "CเDIGO     บ FUNCIONตRIO                                       บ  VENDEDOR? บ"
CHDIR "FUN"
linha = 6
SHELL "dir/b/on > c:\temp.tmp"
OPEN temp$ FOR INPUT AS #1
WHILE NOT EOF(1)
LINE INPUT #1, arq$
LOCATE linha, 3: PRINT arq$
b = 1
OPEN arq$ FOR INPUT AS #2
WHILE NOT EOF(2)
LINE INPUT #2, nome$
SELECT CASE b
        CASE 1
                LOCATE linha, 16: PRINT nome$
        CASE 11
                LOCATE linha, 69
                PRINT nome$
        CASE ELSE
        END SELECT
b = b + 1
WEND
CLOSE #2
IF linha = 20 THEN
        barra "PAUSE"
        CLS
        FOR a = 1 TO 23
        LOCATE a, 1
        SELECT CASE a
                CASE 1
                PRINT "ษออออออออออออหอออออออออออออออออออออออออออออออออออออออออออออออออออหออออออออออออป"
                CASE 2
                PRINT "บ            บ                                                   บ            บ"
                CASE 3
                        PRINT "ฬออออออออออออฮอออออออออออออออออออออออออออออออออออออออออออออออออออฮออออออออออออน"
                CASE 5
                        PRINT "ฬออออออออออออฮอออออออออออออออออออออออออออออออออออออออออออออออออออฮออออออออออออน"
                CASE 21
                        PRINT "ฬออออออออออออสอออออออออออออออออออออออออออออออออออออออออออออออออออสออออออออออออน"
                CASE 22
                        PRINT "บ                                                                             บ"
                CASE 23
                        PRINT "ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ"
                CASE ELSE
                        PRINT "บ            บ                                                   บ            บ"
                END SELECT
        NEXT a
        COLOR 15, 4: LOCATE 2, 3: PRINT "InfoBrasil บ ContEmp 1.0 - Cadastro de Vendedores              บ "; DATE$
        COLOR 15, 1
        LOCATE 4, 3:    PRINT "CเDIGO     บ FUNCIONตRIO                                       บ  VENDEDOR? บ"
        linha = 6
ELSE
linha = linha + 1
END IF
WEND
CLOSE #1
KILL temp$
LOCATE 22, 3: INPUT "Entre com o cขdigo do funcion rio (NADA para cancelar): ", canc$
IF canc$ = "" THEN CHDIR "..": principal
CLS
FOR a = 1 TO 23
LOCATE a, 1
SELECT CASE a
        CASE 1
        PRINT "ษออออออออออออหอออออออออออออออออออออออออออออออออออออออออออออออออออหออออออออออออป"
        CASE 2
        PRINT "บ            บ                                                   บ            บ"
        CASE 3
                PRINT "ฬออออออออออออสอออออออออออออออออออออออออออออออออออออออออออออออออออสออออออออออออน"
        CASE 21
                PRINT "ฬอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออน"
        CASE 23
                PRINT "ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ"
        CASE ELSE
                PRINT "บ                                                                             บ"
        END SELECT
NEXT a
COLOR 15, 4: LOCATE 2, 3: PRINT "InfoBrasil บ ContEmp 1.0 - Controle de Empresas                บ "; DATE$
COLOR 15, 1
linha = 4
OPEN canc$ FOR INPUT AS #1
WHILE NOT EOF(1)
LINE INPUT #1, a$
LOCATE linha, 3
PRINT a$
linha = linha + 1
WEND
CLOSE #1
5 barra "LIMPAR"
LOCATE 22, 3: INPUT "Deseja transform -lo em um vendedor (S/N)? ", sn$
sn$ = UCASE$(sn$)
SELECT CASE sn$
CASE "S"
        linha = 1
        OPEN canc$ FOR INPUT AS #1
        OPEN temp$ FOR OUTPUT AS #2
        WHILE NOT EOF(1)
        LINE INPUT #1, a$
        IF linha = 11 THEN
                PRINT #2, "SIM"
        ELSE
                PRINT #2, a$
        END IF
        linha = linha + 1
        WEND
        CLOSE #1, #2
        KILL canc$
        NAME temp$ AS canc$
CASE "N"
        linha = 1
        OPEN canc$ FOR INPUT AS #1
        OPEN temp$ FOR OUTPUT AS #2
        WHILE NOT EOF(1)
        LINE INPUT #1, a$
        IF linha = 11 THEN
                PRINT #2, "NAO"
        ELSE
                PRINT #2, a$
        END IF
        linha = linha + 1
        WEND
        CLOSE #1, #2
        KILL canc$
        NAME temp$ AS canc$
CASE ELSE
        barra "limpar"
        barra "ErrorEsc"
        GOTO 5
END SELECT
CHDIR ".."
principal
END SUB

SUB Ended
CLS
FOR a = 1 TO 23
LOCATE a, 1
SELECT CASE a
        CASE 1
        PRINT "ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป"
        CASE 23
                PRINT "ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ"
        CASE ELSE
                PRINT "บ                                                                             บ"
        END SELECT
NEXT a
FOR a = 10 TO 12 STEP 2
LOCATE a, 31
SELECT CASE a
        CASE 10
                PRINT "Nฦo se esquea de"
        CASE 12
                PRINT "fazer back up's!"
        END SELECT
NEXT a
END
END SUB

SUB menucadastro
FOR a = 5 TO 14
LOCATE a, 8
SELECT CASE a
        CASE 5
                PRINT "ษออออออออออออออออออป"
        CASE 6
                PRINT "บ   Menu Cadastro  บ"
        CASE 7
                PRINT "ฬออออออออออออออออออน"
        CASE 8
                PRINT "บ 1 - Fornecedores บ"
        CASE 9
                PRINT "บ 2 - Produtos     บ"
        CASE 10
                PRINT "บ 3 - Funcion rios บ"
        CASE 11
                PRINT "บ 4 - Vendedores   บ"
        CASE 12
                PRINT "บ 5 - Clientes     บ"
        CASE 13
                PRINT "บ 0 - Voltar       บ"
        CASE 14
                PRINT "ศออออออออออออออออออผ"
END SELECT
NEXT a
LOCATE 22, 3: INPUT "Entre com sua escolha: ", esc$
SELECT CASE esc$
        CASE "0"
                principal
        CASE "1"
                cadastrofornecedores
        CASE "2"
                cadastroprodutos
        CASE "3"
                cadastrofuncionarios
        CASE "4"
                cadastrovendedores
        CASE "5"
                cadastroclientes
        CASE ELSE
                msgbox "SEM_OPCAO", 1
        END SELECT

END SUB

SUB menuopcoes
FOR a = 5 TO 12
LOCATE a, 8
SELECT CASE a
        CASE 5
                PRINT "ษออออออออออออออออออป"
        CASE 6
                PRINT "บ    Menu Opไes   บ"
        CASE 7
                PRINT "ฬออออออออออออออออออน"
        CASE 8
                PRINT "บ 1 - Empresa      บ"
        CASE 9
                PRINT "บ 2 - Impressora   บ"
        CASE 10
                PRINT "บ 3 - Senha        บ"
        CASE 11
                PRINT "บ 0 - Voltar       บ"
        CASE 12
                PRINT "ศออออออออออออออออออผ"
END SELECT
NEXT a
LOCATE 22, 3: INPUT "Entre com sua escolha: ", esc$
SELECT CASE esc$
        CASE "0"
                principal
        CASE ELSE
                msgbox "SEM_OPCAO", 1
        END SELECT

END SUB

SUB menuprincipal
FOR a = 5 TO 15
LOCATE a, 5
SELECT CASE a
        CASE 5
                PRINT "ษออออออออออออออออป"
        CASE 6
                PRINT "บ Menu Principal บ"
        CASE 7
                PRINT "ฬออออออออออออออออน"
        CASE 8
                PRINT "บ 1 - Cadastro   บ"
        CASE 9
                PRINT "บ 2 - Remocao    บ"
        CASE 10
                PRINT "บ 3 - Relatขrios บ "
        CASE 11
                PRINT "บ 4 - Vendas     บ"
        CASE 12
                PRINT "บ 5 - Opcoes     บ"
        CASE 13
                PRINT "บ 6 - Programas  บ"
        CASE 14
                PRINT "บ 0 - Sair       บ"
        CASE 15
                PRINT "ศออออออออออออออออผ"
END SELECT
NEXT a
LOCATE 22, 3: INPUT "Entre com sua escolha: ", esc$
SELECT CASE esc$
        CASE "0"
                msgbox "FIM", 0
        CASE "1"
                menucadastro
        CASE "2"
                menuremocao
        CASE "3"
                menurelatorios
        CASE "4"
                menuvendas
        CASE "5"
                menuopcoes
        CASE "6"
                menuprogramas
        CASE ELSE
                msgbox "SEM_OPCAO", 0
        END SELECT
END SUB

SUB menuprogramas
FOR a = 5 TO 11
LOCATE a, 8
SELECT CASE a
        CASE 5
                PRINT "ษออออออออออออออออออป"
        CASE 6
                PRINT "บ  Menu Programas  บ"
        CASE 7
                PRINT "ฬออออออออออออออออออน"
        CASE 8
                PRINT "บ 1 - Back up      บ"
        CASE 9
                PRINT "บ 2 - Restore      บ"
        CASE 10
                PRINT "บ 0 - Voltar       บ"
        CASE 11
                PRINT "ศออออออออออออออออออผ"
END SELECT
NEXT a
LOCATE 22, 3: INPUT "Entre com sua escolha: ", esc$
SELECT CASE esc$
        CASE "0"
                principal
        CASE ELSE
                msgbox "SEM_OPCAO", 1
        END SELECT

END SUB

SUB menurelatorios
FOR a = 5 TO 14
LOCATE a, 8
SELECT CASE a
        CASE 5
                PRINT "ษออออออออออออออออออป"
        CASE 6
                PRINT "บ Menu Relatขrios  บ"
        CASE 7
                PRINT "ฬออออออออออออออออออน"
        CASE 8
                PRINT "บ 1 - Fornecedores บ"
        CASE 9
                PRINT "บ 2 - Produtos     บ"
        CASE 10
                PRINT "บ 3 - Funcion rios บ"
        CASE 11
                PRINT "บ 4 - Vendedores   บ"
        CASE 12
                PRINT "บ 5 - Clientes     บ"
        CASE 13
                PRINT "บ 0 - Voltar       บ"
        CASE 14
                PRINT "ศออออออออออออออออออผ"
END SELECT
NEXT a
LOCATE 22, 3: INPUT "Entre com sua escolha: ", esc$
SELECT CASE esc$
        CASE "0"
                principal
        CASE ELSE
                msgbox "SEM_OPCAO", 1
        END SELECT
END SUB

SUB menuremocao
FOR a = 5 TO 13
LOCATE a, 8
SELECT CASE a
        CASE 5
                PRINT "ษออออออออออออออออออป"
        CASE 6
                PRINT "บ    Menu Remoฦo  บ"
        CASE 7
                PRINT "ฬออออออออออออออออออน"
        CASE 8
                PRINT "บ 1 - Fornecedores บ"
        CASE 9
                PRINT "บ 2 - Produtos     บ"
        CASE 10
                PRINT "บ 3 - Funcion rios บ"
        CASE 11
                PRINT "บ 4 - Clientes     บ"
        CASE 12
                PRINT "บ 0 - Voltar       บ"
        CASE 13
                PRINT "ศออออออออออออออออออผ"
END SELECT
NEXT a
LOCATE 22, 3: INPUT "Entre com sua escolha: ", esc$
SELECT CASE esc$
        CASE "0"
                principal
        CASE "1"
                removerfornecedores
        CASE "2"
                removerprodutos
        CASE "3"
                removerfuncionarios
        CASE "4"
                removerclientes
        CASE ELSE
                msgbox "SEM_OPCAO", 1
        END SELECT

END SUB

SUB menuvendas
FOR a = 5 TO 15
LOCATE a, 20
SELECT CASE a
        CASE 5
                PRINT "ษออออออออออออออออออป"
        CASE 6
                PRINT "บ    Menu Vendas   บ"
        CASE 7
                PRINT "ฬออออออออออออออออออน"
        CASE 8
                PRINT "บ 1 - Vendas Balcaoบ"
        CASE 9
                PRINT "บ 2 - Cosignaฦo   บ"
        CASE 10
                PRINT "ฬอออออRelatขrioออออน"
        CASE 11
                PRINT "บ 3 - Por data     บ"
        CASE 12
                PRINT "บ 4 - Por vendedor บ"
        CASE 13
                PRINT "บ 5 - Fechar caixa บ"
        CASE 14
                PRINT "บ 0 - Voltar       บ"
        CASE 15
                PRINT "ศออออออออออออออออออผ"
END SELECT
NEXT a
LOCATE 22, 3: INPUT "Entre com sua escolha: ", esc$
SELECT CASE esc$
        CASE "0"
                principal
        CASE "1"
                CHDIR "VENDAS"
                CLS
                SHELL "vendas"
                CHDIR "c:\contemp"
                principal
        CASE ELSE
                msgbox "SEM_OPCAO", 1
        END SELECT

END SUB

SUB msgbox (entrada$, a)
'ฐ ฑ ฒ ณ ด น บ ป ผ ฟ ภ ม ย ร ฤ ล ศ ษ ส ห ฬ อ ฮ
entrada$ = UCASE$(entrada$)
SELECT CASE entrada$
        CASE "EXCLUIDO"
                LOCATE 11, 22: PRINT "ษอออออออออออออออออออออออป"
                LOCATE 12, 22: PRINT "บ Excluido com sucesso! บ"
                LOCATE 13, 22: PRINT "ศอออออออออออออออออออออออผ"
                r$ = INPUT$(1)
                principal
        CASE "SEM_OPCAO"
                LOCATE 11, 22: PRINT "ษอออออออออออออออออออออออออออออออออออป"
                LOCATE 12, 22: PRINT "บ Nฦo foi selecionado nenhuma opcao บ"
                LOCATE 13, 22: PRINT "ศอออออออออออออออออออออออออออออออออออผ"
                r$ = INPUT$(1)
                principal
        CASE "MAIS_UM"
                LOCATE 16, 22: PRINT "ษอออออออออออออออออออออออออออออออป"
                LOCATE 17, 22: PRINT "บ Deseja adicioar outro dado?   บ"
                LOCATE 18, 22: PRINT "ศอออออออออออออออออออออออออออออออผ"
                LOCATE 17, 52: INPUT "", sn$
                sn$ = UCASE$(sn$)
                SELECT CASE sn$
                        CASE "S"
                                SELECT CASE a
                                        CASE 0
                                                cadastrofornecedores
                                        CASE 1
                                                cadastroprodutos
                                        END SELECT
                        CASE ELSE
                                principal
                        END SELECT
        CASE "FIM"
                LOCATE 11, 22: PRINT "ษอออออออออออออออออออออออออออออออออออป"
                LOCATE 12, 22: PRINT "บ Voc tem certeza que quer sair?   บ"
                LOCATE 13, 22: PRINT "ศอออออออออออออออออออออออออออออออออออผ"
                LOCATE 12, 56: INPUT "", sn$
                sn$ = UCASE$(sn$)
                SELECT CASE sn$
                        CASE "S"
                                Ended
                        CASE ELSE
                                principal
                        END SELECT
        CASE ELSE
        END SELECT
END SUB

SUB passwords
Y = 20
X = 34

b = 1
OPEN "conf.txt" FOR INPUT AS #1
WHILE NOT EOF(1)
LINE INPUT #1, a$
IF b = 6 THEN GOTO 1
b = b + 1
WEND
1 CLOSE #1
CORRECT.PASSWORD$ = a$

CLS
password:
COLOR 1, 1
SCREEN 0
CLS

GOSUB display

password$ = ""
star$ = ""

DO

key.press$ = INPUT$(1)

IF key.press$ = CHR$(13) THEN GOTO check.password



IF key.press$ = CHR$(8) THEN
star2$ = ""
password2$ = ""
FOR q = 1 TO LEN(star$)
star2$ = star2$ + " "
NEXT q
FOR q = 1 TO LEN(password$)
password2$ = password2$ + " "
NEXT q
IF LEN(password$) > 0 THEN
password$ = LEFT$(password$, LEN(password$) - 1)
star$ = LEFT$(star$, LEN(star$) - 1)
LOCATE Y, X
PRINT star2$
LOCATE Y, X
PRINT star$
ELSE BEEP
END IF
END IF


IF key.press$ <> CHR$(13) THEN
IF key.press$ <> CHR$(8) THEN
IF LEN(password$) < 20 THEN
password$ = password$ + key.press$
star$ = ""
V = LEN(password$)
FOR c = 1 TO V
star$ = star$ + "*"
NEXT c
LOCATE Y, X
PRINT star$
ELSE BEEP
END IF
END IF
END IF

LOOP



check.password:

CORRECT.PASSWORD$ = UCASE$(CORRECT.PASSWORD$)
password$ = UCASE$(password$)

IF password$ = CORRECT.PASSWORD$ THEN
principal
ELSE
LOCATE 19, 22
PRINT "ษออออออออออออออออออออออออออออออออป"
LOCATE 20, 22
PRINT "บ        Senha Incorreta!        บ"
LOCATE 21, 22
PRINT "ศออออออออออออออออออออออออออออออออผ"
BEEP
DO: LOOP WHILE INKEY$ = ""
GOTO password
END IF
END

display:
COLOR 0, 7
PRINT "                         O Programa de Senhas Versฦo 1.0                        "
COLOR 15, 1
PRINT "                                                                                "
PRINT "                    CCCCC  OOO  N   N TTTTT EEEEE M   M PPPPP"
PRINT "                    C     O   O NN  N   T   E     MM MM P   P"
PRINT "                    C     O   O N N N   T   EEEEE M M M PPPPP"
PRINT "                    C     O   O N  NN   T   E     M   M P    "
PRINT "                    CCCCC  OOO  N   N   T   EEEEE M   M P    "
PRINT "                                                                                "
PRINT "                               Por Ygor Lazaro"
PRINT
PRINT "                                                                                "
PRINT "                                                                                "
PRINT "                                                                                "
PRINT "                                                                                "
PRINT "                                                                                "
PRINT "                                                                                "
LOCATE 19, 22
PRINT "ษออออออออออออออออออออออออออออออออป"
LOCATE 20, 22
PRINT "บ    Senha:                      บ"
LOCATE 21, 22
PRINT "ศออออออออออออออออออออออออออออออออผ"

RETURN

END SUB

SUB principal
COLOR 15, 1
CLS
FOR a = 1 TO 23
LOCATE a, 1
SELECT CASE a
        CASE 1
        PRINT "ษออออออออออออหอออออออออออออออออออออออออออออออออออออออออออออออออออหออออออออออออป"
        CASE 2
        PRINT "บ            บ                                                   บ            บ"
        CASE 3
                PRINT "ฬออออออออออออสอออออออออออออออออออออออออออออออออออออออออออออออออออสออออออออออออน"
        CASE 21
                PRINT "ฬอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออน"
        CASE 23
                PRINT "ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ"
        CASE ELSE
                PRINT "บ                                                                             บ"
        END SELECT
NEXT a
COLOR 15, 4: LOCATE 2, 3: PRINT "InfoBrasil บ ContEmp 1.0 - Controle de Empresas                บ "; DATE$
COLOR 15, 1
menuprincipal
END SUB

SUB removerclientes
CLS
FOR a = 1 TO 23
LOCATE a, 1
SELECT CASE a
        CASE 1
        PRINT "ษออออออออออออหอออออออออออออออออออออออออออออออออออออออออออออออออออหออออออออออออป"
        CASE 2
        PRINT "บ            บ                                                   บ            บ"
        CASE 3
                PRINT "ฬออออออออออออฮอออออออออออออออออออออออออออออออออออออออออออออออออออฮออออออออออออน"
        CASE 5
                PRINT "ฬออออออออออออฮอออออออออออออออออออออออออออออออออออออออออออออออออออฮออออออออออออน"
        CASE 21
                PRINT "ฬออออออออออออสอออออออออออออออออออออออออออออออออออออออออออออออออออสออออออออออออน"
        CASE 22
                PRINT "บ                                                                             บ"
        CASE 23
                PRINT "ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ"
        CASE ELSE
                PRINT "บ            บ                                                   บ            บ"
        END SELECT
NEXT a
COLOR 15, 4: LOCATE 2, 3: PRINT "InfoBrasil บ ContEmp 1.0 - Remoฦo de clientes                 บ "; DATE$
COLOR 15, 1
LOCATE 4, 3:    PRINT "CเDIGO     บ CLIENTES                                                       บ"
CHDIR "CLI"
linha = 6
SHELL "dir/b/on > c:\temp.tmp"
OPEN temp$ FOR INPUT AS #1
WHILE NOT EOF(1)
LINE INPUT #1, arq$
LOCATE linha, 3: PRINT arq$
b = 1
OPEN arq$ FOR INPUT AS #2
WHILE NOT EOF(2)
LINE INPUT #2, nome$
SELECT CASE b
        CASE 1
                LOCATE linha, 16: PRINT nome$
        CASE ELSE
        END SELECT
b = b + 1
WEND
CLOSE #2
IF linha = 20 THEN
        barra "PAUSE"
        CLS
        FOR a = 1 TO 23
        LOCATE a, 1
        SELECT CASE a
                CASE 1
                PRINT "ษออออออออออออหอออออออออออออออออออออออออออออออออออออออออออออออออออหออออออออออออป"
                CASE 2
                PRINT "บ            บ                                                   บ            บ"
                CASE 3
                        PRINT "ฬออออออออออออฮอออออออออออออออออออออออออออออออออออออออออออออออออออฮออออออออออออน"
                CASE 5
                        PRINT "ฬออออออออออออฮอออออออออออออออออออออออออออออออออออออออออออออออออออฮออออออออออออน"
                CASE 21
                        PRINT "ฬออออออออออออสอออออออออออออออออออออออออออออออออออออออออออออออออออสออออออออออออน"
                CASE 22
                        PRINT "บ                                                                             บ"
                CASE 23
                        PRINT "ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ"
                CASE ELSE
                        PRINT "บ            บ                                                   บ            บ"
                END SELECT
        NEXT a
        COLOR 15, 4: LOCATE 2, 3: PRINT "InfoBrasil บ ContEmp 1.0 - Remoฦo de Clientes                 บ "; DATE$
        COLOR 15, 1
        LOCATE 4, 3:    PRINT "CเDIGO     บ CLIENTES                                                       บ"
        linha = 6
ELSE
linha = linha + 1
END IF
WEND
CLOSE #1
KILL temp$
LOCATE 22, 3: INPUT "Entre com o cขdigo do clientes (NADA para cancelar): ", canc$
IF canc$ = "" THEN CHDIR "..": principal
CLS
FOR a = 1 TO 23
LOCATE a, 1
SELECT CASE a
        CASE 1
        PRINT "ษออออออออออออหอออออออออออออออออออออออออออออออออออออออออออออออออออหออออออออออออป"
        CASE 2
        PRINT "บ            บ                                                   บ            บ"
        CASE 3
                PRINT "ฬออออออออออออสอออออออออออออออออออออออออออออออออออออออออออออออออออสออออออออออออน"
        CASE 21
                PRINT "ฬอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออน"
        CASE 23
                PRINT "ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ"
        CASE ELSE
                PRINT "บ                                                                             บ"
        END SELECT
NEXT a
COLOR 15, 4: LOCATE 2, 3: PRINT "InfoBrasil บ ContEmp 1.0 - Controle de Empresas                บ "; DATE$
COLOR 15, 1
linha = 4
OPEN canc$ FOR INPUT AS #1
WHILE NOT EOF(1)
LINE INPUT #1, a$
LOCATE linha, 3
PRINT a$
linha = linha + 1
WEND
CLOSE #1
10 barra "LIMPAR"
LOCATE 22, 3: INPUT "Voc deseja excluir este clientes (S/N)? ", sn$
sn$ = UCASE$(sn$)
SELECT CASE sn$
        CASE "S"
                KILL canc$
                CHDIR ".."
                msgbox "EXCLUIDO", 0
        CASE "N"
                CHDIR ".."
                principal
        CASE ELSE
                GOTO 10
        END SELECT
CHDIR ".."
principal

END SUB

SUB removerfornecedores
CLS
FOR a = 1 TO 23
LOCATE a, 1
SELECT CASE a
        CASE 1
        PRINT "ษออออออออออออหอออออออออออออออออออออออออออออออออออออออออออออออออออหออออออออออออป"
        CASE 2
        PRINT "บ            บ                                                   บ            บ"
        CASE 3
                PRINT "ฬออออออออออออฮอออออออออออออออออออออออออออออออออออออออออออออออออออฮออออออออออออน"
        CASE 5
                PRINT "ฬออออออออออออฮอออออออออออออออออออออออออออออออออออออออออออออออออออฮออออออออออออน"
        CASE 21
                PRINT "ฬออออออออออออสอออออออออออออออออออออออออออออออออออออออออออออออออออสออออออออออออน"
        CASE 22
                PRINT "บ                                                                             บ"
        CASE 23
                PRINT "ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ"
        CASE ELSE
                PRINT "บ            บ                                                   บ            บ"
        END SELECT
NEXT a
COLOR 15, 4: LOCATE 2, 3: PRINT "InfoBrasil บ ContEmp 1.0 - Remoฦo de fornecedores             บ "; DATE$
COLOR 15, 1
LOCATE 4, 3:    PRINT "CเDIGO     บ FORNECEDOR                                                     บ"
CHDIR "FOR"
linha = 6
SHELL "dir/b/on > c:\temp.tmp"
OPEN temp$ FOR INPUT AS #1
WHILE NOT EOF(1)
LINE INPUT #1, arq$
LOCATE linha, 3: PRINT arq$
b = 1
OPEN arq$ FOR INPUT AS #2
WHILE NOT EOF(2)
LINE INPUT #2, nome$
SELECT CASE b
        CASE 1
                LOCATE linha, 16: PRINT nome$
        CASE ELSE
        END SELECT
b = b + 1
WEND
CLOSE #2
IF linha = 20 THEN
        barra "PAUSE"
        CLS
        FOR a = 1 TO 23
        LOCATE a, 1
        SELECT CASE a
                CASE 1
                PRINT "ษออออออออออออหอออออออออออออออออออออออออออออออออออออออออออออออออออหออออออออออออป"
                CASE 2
                PRINT "บ            บ                                                   บ            บ"
                CASE 3
                        PRINT "ฬออออออออออออฮอออออออออออออออออออออออออออออออออออออออออออออออออออฮออออออออออออน"
                CASE 5
                        PRINT "ฬออออออออออออฮอออออออออออออออออออออออออออออออออออออออออออออออออออฮออออออออออออน"
                CASE 21
                        PRINT "ฬออออออออออออสอออออออออออออออออออออออออออออออออออออออออออออออออออสออออออออออออน"
                CASE 22
                        PRINT "บ                                                                             บ"
                CASE 23
                        PRINT "ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ"
                CASE ELSE
                        PRINT "บ            บ                                                   บ            บ"
                END SELECT
        NEXT a
        COLOR 15, 4: LOCATE 2, 3: PRINT "InfoBrasil บ ContEmp 1.0 - Remoฦo de Fornecedores             บ "; DATE$
        COLOR 15, 1
        LOCATE 4, 3:    PRINT "CเDIGO     บ FORNECEDOR                                                     บ"
        linha = 6
ELSE
linha = linha + 1
END IF
WEND
CLOSE #1
KILL temp$
LOCATE 22, 3: INPUT "Entre com o cขdigo do fornecedor (NADA para cancelar): ", canc$
IF canc$ = "" THEN CHDIR "..": principal
CLS
FOR a = 1 TO 23
LOCATE a, 1
SELECT CASE a
        CASE 1
        PRINT "ษออออออออออออหอออออออออออออออออออออออออออออออออออออออออออออออออออหออออออออออออป"
        CASE 2
        PRINT "บ            บ                                                   บ            บ"
        CASE 3
                PRINT "ฬออออออออออออสอออออออออออออออออออออออออออออออออออออออออออออออออออสออออออออออออน"
        CASE 21
                PRINT "ฬอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออน"
        CASE 23
                PRINT "ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ"
        CASE ELSE
                PRINT "บ                                                                             บ"
        END SELECT
NEXT a
COLOR 15, 4: LOCATE 2, 3: PRINT "InfoBrasil บ ContEmp 1.0 - Controle de Empresas                บ "; DATE$
COLOR 15, 1
linha = 4
OPEN canc$ FOR INPUT AS #1
WHILE NOT EOF(1)
LINE INPUT #1, a$
LOCATE linha, 3
PRINT a$
linha = linha + 1
WEND
CLOSE #1
7 barra "LIMPAR"
LOCATE 22, 3: INPUT "Voc deseja excluir este fornecedor (S/N)? ", sn$
sn$ = UCASE$(sn$)
SELECT CASE sn$
        CASE "S"
                KILL canc$
                CHDIR ".."
                msgbox "EXCLUIDO", 0
        CASE "N"
                CHDIR ".."
                principal
        CASE ELSE
                GOTO 7
        END SELECT
CHDIR ".."
principal
END SUB

SUB removerfuncionarios
CLS
FOR a = 1 TO 23
LOCATE a, 1
SELECT CASE a
        CASE 1
        PRINT "ษออออออออออออหอออออออออออออออออออออออออออออออออออออออออออออออออออหออออออออออออป"
        CASE 2
        PRINT "บ            บ                                                   บ            บ"
        CASE 3
                PRINT "ฬออออออออออออฮอออออออออออออออออออออออออออออออออออออออออออออออออออฮออออออออออออน"
        CASE 5
                PRINT "ฬออออออออออออฮอออออออออออออออออออออออออออออออออออออออออออออออออออฮออออออออออออน"
        CASE 21
                PRINT "ฬออออออออออออสอออออออออออออออออออออออออออออออออออออออออออออออออออสออออออออออออน"
        CASE 22
                PRINT "บ                                                                             บ"
        CASE 23
                PRINT "ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ"
        CASE ELSE
                PRINT "บ            บ                                                   บ            บ"
        END SELECT
NEXT a
COLOR 15, 4: LOCATE 2, 3: PRINT "InfoBrasil บ ContEmp 1.0 - Remoฦo de Funcion rios             บ "; DATE$
COLOR 15, 1
LOCATE 4, 3:    PRINT "CเDIGO     บ FUNCIONตRIOS                                                   บ"
CHDIR "FUN"
linha = 6
SHELL "dir/b/on > c:\temp.tmp"
OPEN temp$ FOR INPUT AS #1
WHILE NOT EOF(1)
LINE INPUT #1, arq$
LOCATE linha, 3: PRINT arq$
b = 1
OPEN arq$ FOR INPUT AS #2
WHILE NOT EOF(2)
LINE INPUT #2, nome$
SELECT CASE b
        CASE 1
                LOCATE linha, 16: PRINT nome$
        CASE ELSE
        END SELECT
b = b + 1
WEND
CLOSE #2
IF linha = 20 THEN
        barra "PAUSE"
        CLS
        FOR a = 1 TO 23
        LOCATE a, 1
        SELECT CASE a
                CASE 1
                PRINT "ษออออออออออออหอออออออออออออออออออออออออออออออออออออออออออออออออออหออออออออออออป"
                CASE 2
                PRINT "บ            บ                                                   บ            บ"
                CASE 3
                        PRINT "ฬออออออออออออฮอออออออออออออออออออออออออออออออออออออออออออออออออออฮออออออออออออน"
                CASE 5
                        PRINT "ฬออออออออออออฮอออออออออออออออออออออออออออออออออออออออออออออออออออฮออออออออออออน"
                CASE 21
                        PRINT "ฬออออออออออออสอออออออออออออออออออออออออออออออออออออออออออออออออออสออออออออออออน"
                CASE 22
                        PRINT "บ                                                                             บ"
                CASE 23
                        PRINT "ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ"
                CASE ELSE
                        PRINT "บ            บ                                                   บ            บ"
                END SELECT
        NEXT a
        COLOR 15, 4: LOCATE 2, 3: PRINT "InfoBrasil บ ContEmp 1.0 - Remoฦo de Funcion rios             บ "; DATE$
        COLOR 15, 1
        LOCATE 4, 3:    PRINT "CเDIGO     บ FUNCIONตRIOS                                                   บ"
        linha = 6
ELSE
linha = linha + 1
END IF
WEND
CLOSE #1
KILL temp$
LOCATE 22, 3: INPUT "Entre com o cขdigo do funcion rio (NADA para cancelar): ", canc$
IF canc$ = "" THEN CHDIR "..": principal
CLS
FOR a = 1 TO 23
LOCATE a, 1
SELECT CASE a
        CASE 1
        PRINT "ษออออออออออออหอออออออออออออออออออออออออออออออออออออออออออออออออออหออออออออออออป"
        CASE 2
        PRINT "บ            บ                                                   บ            บ"
        CASE 3
                PRINT "ฬออออออออออออสอออออออออออออออออออออออออออออออออออออออออออออออออออสออออออออออออน"
        CASE 21
                PRINT "ฬอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออน"
        CASE 23
                PRINT "ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ"
        CASE ELSE
                PRINT "บ                                                                             บ"
        END SELECT
NEXT a
COLOR 15, 4: LOCATE 2, 3: PRINT "InfoBrasil บ ContEmp 1.0 - Controle de Empresas                บ "; DATE$
COLOR 15, 1
linha = 4
OPEN canc$ FOR INPUT AS #1
WHILE NOT EOF(1)
LINE INPUT #1, a$
LOCATE linha, 3
PRINT a$
linha = linha + 1
WEND
CLOSE #1
9 barra "LIMPAR"
LOCATE 22, 3: INPUT "Voc deseja excluir este funcion rio (S/N)? ", sn$
sn$ = UCASE$(sn$)
SELECT CASE sn$
        CASE "S"
                KILL canc$
                CHDIR ".."
                msgbox "EXCLUIDO", 0
        CASE "N"
                CHDIR ".."
                principal
        CASE ELSE
                GOTO 9
        END SELECT
CHDIR ".."
principal

END SUB

SUB removerprodutos
CLS
FOR a = 1 TO 23
LOCATE a, 1
SELECT CASE a
        CASE 1
        PRINT "ษออออออออออออหอออออออออออออออออออออออออออออออออออออออออออออออออออหออออออออออออป"
        CASE 2
        PRINT "บ            บ                                                   บ            บ"
        CASE 3
                PRINT "ฬออออออออออออฮอออออออออออออออออออออออออออออออออออออออออออออออออออฮออออออออออออน"
        CASE 5
                PRINT "ฬออออออออออออฮอออออออออออออออออออออออออออออออออออออออออออออออออออฮออออออออออออน"
        CASE 21
                PRINT "ฬออออออออออออสอออออออออออออออออออออออออออออออออออออออออออออออออออสออออออออออออน"
        CASE 22
                PRINT "บ                                                                             บ"
        CASE 23
                PRINT "ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ"
        CASE ELSE
                PRINT "บ            บ                                                   บ            บ"
        END SELECT
NEXT a
COLOR 15, 4: LOCATE 2, 3: PRINT "InfoBrasil บ ContEmp 1.0 - Remoฦo de Produtos                 บ "; DATE$
COLOR 15, 1
LOCATE 4, 3:    PRINT "CเDIGO     บ PRODUTO                                                        บ"
CHDIR "PRO"
linha = 6
SHELL "dir/b/on > c:\temp.tmp"
OPEN temp$ FOR INPUT AS #1
WHILE NOT EOF(1)
LINE INPUT #1, arq$
LOCATE linha, 3: PRINT arq$
b = 1
OPEN arq$ FOR INPUT AS #2
WHILE NOT EOF(2)
LINE INPUT #2, nome$
SELECT CASE b
        CASE 1
                LOCATE linha, 16: PRINT nome$
        CASE ELSE
        END SELECT
b = b + 1
WEND
CLOSE #2
IF linha = 20 THEN
        barra "PAUSE"
        CLS
        FOR a = 1 TO 23
        LOCATE a, 1
        SELECT CASE a
                CASE 1
                PRINT "ษออออออออออออหอออออออออออออออออออออออออออออออออออออออออออออออออออหออออออออออออป"
                CASE 2
                PRINT "บ            บ                                                   บ            บ"
                CASE 3
                        PRINT "ฬออออออออออออฮอออออออออออออออออออออออออออออออออออออออออออออออออออฮออออออออออออน"
                CASE 5
                        PRINT "ฬออออออออออออฮอออออออออออออออออออออออออออออออออออออออออออออออออออฮออออออออออออน"
                CASE 21
                        PRINT "ฬออออออออออออสอออออออออออออออออออออออออออออออออออออออออออออออออออสออออออออออออน"
                CASE 22
                        PRINT "บ                                                                             บ"
                CASE 23
                        PRINT "ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ"
                CASE ELSE
                        PRINT "บ            บ                                                   บ            บ"
                END SELECT
        NEXT a
        COLOR 15, 4: LOCATE 2, 3: PRINT "InfoBrasil บ ContEmp 1.0 - Remoฦo de Produtos                 บ "; DATE$
        COLOR 15, 1
        LOCATE 4, 3:    PRINT "CเDIGO     บ PRODUTO                                                        บ"
        linha = 6
ELSE
linha = linha + 1
END IF
WEND
CLOSE #1
KILL temp$
LOCATE 22, 3: INPUT "Entre com o cขdigo do produto (NADA para cancelar): ", canc$
IF canc$ = "" THEN CHDIR "..": principal
CLS
FOR a = 1 TO 23
LOCATE a, 1
SELECT CASE a
        CASE 1
        PRINT "ษออออออออออออหอออออออออออออออออออออออออออออออออออออออออออออออออออหออออออออออออป"
        CASE 2
        PRINT "บ            บ                                                   บ            บ"
        CASE 3
                PRINT "ฬออออออออออออสอออออออออออออออออออออออออออออออออออออออออออออออออออสออออออออออออน"
        CASE 21
                PRINT "ฬอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออน"
        CASE 23
                PRINT "ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ"
        CASE ELSE
                PRINT "บ                                                                             บ"
        END SELECT
NEXT a
COLOR 15, 4: LOCATE 2, 3: PRINT "InfoBrasil บ ContEmp 1.0 - Controle de Empresas                บ "; DATE$
COLOR 15, 1
linha = 4
OPEN canc$ FOR INPUT AS #1
WHILE NOT EOF(1)
LINE INPUT #1, a$
LOCATE linha, 3
PRINT a$
linha = linha + 1
WEND
CLOSE #1
8 barra "LIMPAR"
LOCATE 22, 3: INPUT "Voc deseja excluir este produto (S/N)? ", sn$
sn$ = UCASE$(sn$)
SELECT CASE sn$
        CASE "S"
                KILL canc$
                CHDIR ".."
                msgbox "EXCLUIDO", 0
        CASE "N"
                CHDIR ".."
                principal
        CASE ELSE
                GOTO 8
        END SELECT
CHDIR ".."
principal

END SUB

