                                 readme.txt
                                                            ver.0.0	2013/05/31
                                                            ver.1.0	2013/06/05
                                                                  萩原　淳一郎


本ファイルでは『Rによるベイジアン動的線型モデル』（朝倉書店，2013年刊行）
の付属ファイルに関する説明を行います。


1. 付属ファイルの内容
　付属ファイルはzip形式で圧縮されており、その内容は以下のようになっています。

○readme.txt
  -- 本ファイルです

○フォルダ「Original files」
  -- 原著のサポートサイト※（http://definetti.uark.edu/~gpetris/dlm/）に存在
     する原著者らによるファイル（※2013年5月現在）
        *** スクリプト：ch2.R, ch3.R, ch4.R, ch5.R
        *** 補助関数：dlmFilterDF.R（関数dlmFilterDF()の定義）, 
                    dlmGibbsDIGt.R（関数dlmGibbsDIGt()の定義）, 
                    outputGap.R（関数gdpGibbs()の定義）, 
                    weighted.quantile.R（関数weighted.quantile()の定義）
        *** データ：GDP.dat, gdp5004.dat, interestRates.dat, invest2.dat, 
                    lakeSuperior.dat, P.dat, qconsum.dat, yields.dat

        上記のスクリプトや補助関数に関するファイルの権利は原著者 (G. Petris,
        S. Petrone, P.Campagnoli) らに帰属します。

○フォルダ「Modified scripts」
  -- 訳出にあたり作成・修正したスクリプト
        *** スクリプト：ch1j.R, ch2j.R, ch3j.R, ch4j.R　（5章分のch5.Rに関して
                    は修正がないため、対応するファイルは存在しません）

○フォルダ「Quick guide」
  -- パッケージdlmに含まれる文書の参考和訳
        *** リファレンスマニュアル：dlm_reference_manual.pdf
        *** ビニエット：dlm_vignettes.pdf

○フォルダ「Errata」
  -- 正誤表
        *** 原著の正誤表（2011/11/29時点）：errata.pdf


2. 訳出にあたり作成・修正したスクリプトの実行について
　実行前にはいくつかの準備が必要ですので、以下の手順に従ってください。

    a. Rの作業ディレクトリの直下に「Datasets」という名称のフォルダを作成し、
      その中に必要なデータファイルをコピーしておく
        2章で使用：qconsum.dat
        3章で使用：lakeSuperior.dat, invest2.dat, gdp5004.dat, P.dat
        4章で使用：lakeSuperior.dat, invest2.dat, gdp5004.dat, 
                   interestRates.dat, yields.dat

    この前提を変更する場合は、スクリプトの中の関数read.table()の引数を各自で
    変更して対応してください。なおRの作業ディレクトリはgetwd()で調べることが
    でき、setwd("xxx")で「xxx」に変更することができます。

    b. 必要なRパッケージをインストールしておく
        dlm：2, 3, 4, 5章で使用
        zoo：3.2.7, 3.3.2, 4.5.2で使用
        nlme：4.1で使用

    Rのパッケージをインストールする方法に関しては、下記サイトや関連書籍を
    参照してください。
        http://cse.naro.affrc.go.jp/takezawa/r-tips/r/08.html

    c. 必要な補助関数を読み込んでおく
        dlmFilterDF()：4.3.2, 4.3.3で使用
        dlmGibbsDIGt()：4.5.3で使用
        gdpGibbs()：4.6.1で使用
        weighted.quantile()：5.3.1で使用

    Rにおけるスクリプトの読み込みに関しては、「xxx.R」をテキストエディタで
    開いてコピーした内容をRのコンソールにペーストしたり、source("xxx.R")で
    Rから直接ファイルを読み込むことで対応可能です。

    d. 最後にスクリプトを実行する
    　スクリプトファイルにおける必要な部分をRに読み込ませて下さい。
    ある部分のスクリプトはそれ以前のスクリプトの実行を前提にしている場合も
    あるため、章毎にファイルの先頭から段階的に実行してゆくのがお勧めです。


3. その他
　スクリプトの実行結果は数値演算ライブラリやRのバージョン等にも依存するため、
書籍の表示と完全に一致しない場合があります。
