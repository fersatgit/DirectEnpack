<h2>Direct Enpack - прямоугольный раскрой</h2>
Это CPG-аддон к CorelDraw для компоновки прямоугольных объектов на лист - аналог <a href=http://www.e-cut.ru/index.php?view=function&functionid=10>функции "прямоугольный раскрой" из пакета ECut</a>.
<h2>Системные требования</h2>
<table  style="font-size:100%"><tr><td>Операционная система:<td>Windows XP или выше
<tr><td>Программное обеспечение:<td>Corel Draw версии 13 или выше
<tr><td>Процессор:<td>с поддержкой SSE4.1. Алгоритм многопоточный - большое количество ядер приветствуется.</table>
<h2>Установка</h2>
<b>x86:</b>  Скопировать файл <a href=>DirectEnpackx86.cpg</a> в каталог "Draw\Plugins", если такого каталога нет - его необходимо создать<p>
<b>x64:</b>  Скопировать файл <a href=>DirectEnpackx64.cpg</a> в каталог "Programs64\Addons"
<h2>Работа с аддоном</h2><ol>
<li>Выделить объекты для компоновки
<li>Запустить аддон при помощи иконки <img src=icon.ico> на панели "Стандарт"
<li>В появившемся диалоговом окне настроить параметры компоновки:<br>
<table><tr><td><img src=1.png><td><ul>
<li>В полях ширина и высота задаётся размер листа в миллиметрах
<li>Отступ - это минимальное расстояние между объектами в миллиметрах
<li>Галочка "Перемешать" рандомизирует порядок объектов перед компоновкой. Это влияет на конечный результат и может его как улучшить так и ухудшить.
<li>Галочка "Добавить рамку" рисует прямоугольник сверхтонким чёрным абрисом вокруг каждого объекта с учётом отступа</ul></table>
<li>После нажатия кнопки "OK" начнётся процесс компоновки. Поиск решения может занять длительное время, но вы можете остановить поиск в любой момент просто закрыв диалоговое окно.
<li>По завершении работы слева будут располагаться объекты, размещённые в лист, а справа - объекты, которые не удалось разместить.</ol>
<p><img src=1.gif>
<h2>Известные проблемы и способы их решения</h2><ul>
<li>Если после запуска диалогового окна настройки панели команд (Инструменты->Параметры->Рабочее пространство->Настройка->Панели команд) выбрать "Отмена" - иконка аддона будет изменена на иконку по-умолчанию. Лечится перезапуском CorelDraw.
<li>Максимальное количество потоков ограничено числом 256. Если появится необходимость увеличить это число - нужно перекомпилировать аддон, увеличив константу <b>MAX_THREADS</b></ul>
<h2>Ссылки</h2><ul>
<li><a href=https://www.researchgate.net/publication/221787421_A_Greedy_Algorithm_with_Forward-Looking_Strategy>A Greedy Algorithm with Forward-Looking Strategy</a> - этот алгоритм взят за основу. В моей реализации изменена оценочная функция.
<li><a href=https://github.com/SebastianBitsch/bin-packing>bin-packing</a> - реализация вышеуказанного алгоритма на Python
<li><a href=https://flatassembler.net/download.php>FASM</a> - компилятор ассемблера для сборки аддона (собиралось в версии 1.73.31)</ul>