# ATS 2019/2020 - Trabalho Prático 1

Resolução das fichas práticas 1 e 2.

Para a execução deste software, começamos por escrever "ghci Parser.hs" na linha de comandos para, desta forma, compilar o programa. Note que é necessário estar dentro da diretoria "Ficha 2" para compilar o programa correto.
De seguida, depois de estar no ambiente do ghci chamamos a função
pMain (função principal do Parser) com o seguinte input:
    "[ Use y , Decl x , [ Decl y , Use x , [ Decl x , Use d ] , Decl y ] , Use x]"

Neste caso, o resultado esperado é o seguinte: "[Use y,Decl y,Use d]". Que é muito parecido com o resultado que o programa obtém, apenas com um pequeno erro (Erro relacionado com o parser).