# ATS 2019/2020 - Trabalho Prático 1

Resolução das fichas práticas 1 e 2.

Para a execução deste software, começamos por escrever "ghci" na linha de comandos para carregar o script de inicialização.
Escrevemos na linha de comandos, PMain (função principal do Parser) e de seguida o input que usamos para testar o correto funcionamento do nosso programa :
pMain"[ Use y , Decl x , [ Decl y , Use x , [ Decl x , Use d ] , Decl y ] , Use x]"

O output que deveriamos ter e que nosso caso funcionou corretamente, seria: 
"[Use y,Decl y,Use d]"


