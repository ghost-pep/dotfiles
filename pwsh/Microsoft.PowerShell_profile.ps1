Import-Module PSReadLine
Import-Module PSColor


# predictive completion of commands
Set-PSReadLineOption -PredictionSource History
Set-PSReadLineOption -PredictionViewStyle ListView
Set-PSReadLineOption -EditMode Windows

# init the prompt
oh-my-posh --init --shell pwsh --config C:\Users\deapa\AppData\Local\Programs\oh-my-posh\themes\half-life.omp.json | Invoke-Expression