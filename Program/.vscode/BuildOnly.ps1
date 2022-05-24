Write-Output " "
Write-Output 'Build process was started'
Write-Output " "

Remove-Item *.err > $null                           # удалить все старые файлы с ошибками
if (Test-Path -Path ./program.exe -PathType Leaf){  # забекапить предыдущую рабочую версию .ехе, если она есть
    Move-Item -path program.exe -destination old-program.exe -Force
}

$var = wfl386 /D2 *.for /FE='program.exe'               # Запустить компилятор и линковщик, результат записать в $var
Remove-Item *.obj                                       # Удаляем все временные .obj файлы

$wrnnum = 0
$errnum = 0

foreach ($i in $var) {                      # Считаем число ошибок и предупреждений в процессе компиляции, выводим их
    if ($i.ToString().IndexOf('ERR') -ne -1) {
        $errnum++
        Write-Output $i.ToString()
    }    
    elseif ($i.ToString().IndexOf('Error!') -ne -1) {
        $errnum++
        Write-Output $i.ToString()
    }   
    elseif ($i.ToString().IndexOf('WRN') -ne -1) {
        $wrnnum++
        Write-Output $i.ToString()
    }
}

if ($errnum -ne 0) {                        # Выводим общее число ошибок и предупреждений, либо говорим об успехе
    Write-Output " "
    Write-Output "Compiler get some errors: $($errnum)"
}
if ($wrnnum -ne 0) {
    Write-Output " "
    Write-Output "Compiler get some warnings: $($wrnnum)"
}
if ($errnum -eq 0) {
    Write-Output "Compile complete. Program may be runned"
}
else {
    Write-Output "Program will not be runned"
}
Write-Output " "
