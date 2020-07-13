      *Divisão de identificação do programa
       Identification Division.
       Program-id. "desafioCorije".
       Author. "Falande Loiseau Etienne".
       Installation. "PC".
       Date-Written. 30/06/2020.
       Date-compiled. 30/06/2020.



      *Divisão para configuração do ambiente
       Environment Division.
       Configuration Section.
           special-names. decimal-point is comma.


      *------ Declaração dos recursos externos
       Input-output Section.
       File-control.
       I-O-control.


      *Declaração de variaveis
       Data Division.



      *-----Variaveis de arquivos
       File Section.



      *----Variaveis de trabalho
       Working-storage Section.

       01  relatorio  occurs  20.
           05 nome                                 pic x(15).
           05 filler                               pic x(03)
              value " - ".
           05 diametro                             pic 9(03).
           05 filler                               pic x(03)
              value " - ".
           05 preco                                pic 9(03)v99.
           05 filler                               pic x(03)
              value " - ".
           05 precoCm2                             pic 9(03)v99.
           05 filler                               pic x(03)
              value " - ".
           05 diferenca                            pic 9(03)v99.
           05 filler                               pic x(03)
              value " - ".
           05 porcent                              pic 9(03).
           05 filler                               pic x(3)
              value " % ".

       77  raio                                    pic 9(03)v99.
       77  areaP                                   pic 9(03)v99.
       77  ind                                     pic 9(02).
       77  qtdPizza                                pic 9(02).
       77  cadastrar                               pic x(01).
       77  controle                                pic x(10).
       77  aux                                     pic 9(03)v99.
       77 nomeAux                                  pic x(10).





      *----Variaveis para comunicação entre programas
       Linkage Section.




      *----Declaração de tela
       Screen Section.




      *Divisão do corpo do programa

      *---------------- APRESENTAÇÃO DO PROBLEMA -----------------------*

      *    Uma empresa de pesquisas online solicitou o desenvolvimento
      *de um software capaz de identificar qual tamanho de pizza
      *apresenta o melhor custo beneficio.
      *    O software deverá receber diversos tamanhos de pizza e seus
      *respectivos preços e ao final exibir um relatório informando em
      *valores absolutos e relativos (percentual) qual a diferença de
      *preços entre as pizzas e deverá informar qual pizza tem o melhor
      *custo benefício.
      *    Entradas:  1. Nome comercial (broto, baby, pequena, média,
      *grande, exagerada, gigante, etc), o tamanho da pizza (diâmetro
      *em centímetros) e respectivo  preço. O software deverá aceitar
      *tantas entradas quanto o usuário deseja comparar, desde que não
      *haja tamanhos duplicados.
      *    Saída: relatório contendo todos os nomes e tamanhos de pizza
      *ordenados do melhor para o pior custo benefício.  O relatório
      *deverá informar o percentual  de diferença do preço de um
      *tamanho para o outro.


       Procedure Division.


           perform inicializa.
           perform processamento.
           perform finaliza.

      * Inicilizacao de variaveis, abertura de arquivos
      * procedimentos que serao realizados apenas uma vez
       inicializa section.

           move   space       to     cadastrar
           move    0          to     qtdPizza
           .
       inicializa-exit.
           exit.

       processamento section.

           move 0 to ind

           perform until cadastrar = "n"

               display erase

               add 1 to ind

               if ind > 20 then

                   display "Vc atingiu o limite de 20 pizzas"

               else

                   display "Informe o nome da pizza "
                   accept nome(ind)

                   display "Informe o diametro "
                   accept diametro(ind)

                   display "Informe o preco "
                   accept preco(ind)

               end-if

      * Chamar o section do cálculo
               perform calculo

               add 1 to qtdPizza

               display "deseja cadastrar mais uma pizza? ('S'/'N')"
               accept cadastrar

           end-perform

      * Chamar o section da ordenação
           perform ordenar

      * Chamar o section do cálculo da porcentagem
           perform porcentagem


      * Exibir a tabela
           perform varying ind from 1 by 1 until ind > qtdPizza
                                              or nome(ind) = space
               display relatorio(ind)

           end-perform

           .
       processamento-exit.
           exit.


       ordenar section.
      * organizando a tabela por custo benefício

           move "trocou" to controle

           perform until controle <> "trocou"
               move 1 to ind
               move "N_trocou" to controle

      *        perform until ind = qtdPizza - 1
               perform until ind = 20 or nome(ind + 1) = space

                   if precoCm2(ind) > precoCm2(ind + 1) then
                       move precoCm2(ind + 1) to aux
                       move precoCm2(ind)   to precoCm2(ind + 1)
                       move aux        to precoCm2(ind)

      *                organizando a variável nome
                       move nome(ind + 1) to nomeAux
                       move nome(ind) to nome(ind + 1)
                       move nomeAux to nome(ind)

      *                organizando a variável diâmetro
                       move diametro(ind + 1) to aux
                       move diametro(ind) to diametro(ind + 1)
                       move aux to diametro(ind)

      *                organizando a variável preco
                       move preco(ind + 1) to aux
                       move preco(ind) to preco(ind + 1)
                       move aux to preco(ind)

                       move "trocou" to controle

                   end-if

                   add 1 to ind
               end-perform

           end-perform

           .
       ordenar-exit.
           exit.


       calculo section.
      * fazendo o cálculo da area e do preço por cm2

           compute raio = diametro(ind)/2

           compute areaP = (raio * raio) * 3,14

           compute precoCm2(ind) = preco(ind) / areaP

           .
       calculo-exit.
           exit.


       porcentagem section.
      * fazer o cálculo da porcentagem de qual será o melhor preço

           move 1 to ind
           move 0 to porcent(ind)
           move 0 to diferenca(ind)

           perform until ind > qtdPizza - 1

               compute diferenca(ind + 1) = precoCm2(ind + 1)
                                          - precoCm2(ind)

               compute porcent(ind + 1) = (diferenca(ind + 1) * 100)
                                       / precoCm2(ind)
               add 1 to ind

           end-perform

           .
       porcentagem-exit.
           exit.


      * Finalização do programa
       finaliza section.
           Stop run
           .
       finaliza-exit.
           exit.
























