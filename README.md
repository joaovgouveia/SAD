# SAD - Sistema Automático de Diagnosticos
Um sistema para a gestão de uma clínica: doenças, médicos e áreas de atuação. Este sistema será utilizado como base para que os usuários possam saber como tratar os seus sintomas. O objetivo é facilitar o diagnóstico inicial e a gestão dos recursos médicos de forma equilibrada. 

## Como rodar:

**Instalar GHCup**
> curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org/ | sh

**Instalar GHC 8.8.4**<br>algumas bibliotecas usadas funcionam melhor com essa versão
> ghcup install ghc 8.8.4

> ghcup set ghc 8.8.4

**Instalar bibliotecas usadas**
> cabal install --lib brick

> cabal install --lib vty

> cabal install --lib aeson  

> cabal install --lib text

> cabal install --lib bytestring

> cabal install --lib time

**Executar o Main**
<br>Dentro do diretório _app_

> **compile o programa**: ghc Main.hs

> **execute o programa**: ./Main