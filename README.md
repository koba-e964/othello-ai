# 概要
`KobaAI`は、理学部情報科学科(IS)の課題で作ることを要求された、オセロのAIである。

## 使い方
### インストール/実行方法
```
$ git clone https://github.com/koba-e964/othello-ai.git koba-ai
```
を実行することで、プロジェクトをダウンロードできる。その後、
```
$ cd koba-ai
$ make
$ ./my-reversi -s 5 -t 100
```
などとすることで実行可能。
## 実装した機能

* ビットボード(盤面を64bit整数2個で表現、各ビットを盤面上の位置と対応させ、黒石と白石の位置を保持する)
(実装はedaxを参考にした。特に合法手の列挙はedaxのソースをそのままHaskellに直した。)
* ネガマックス法
* 時間を指定した先読み(指定された時間内で読めるだけ読む)<br>
 オプション`-t`または`--time`で指定できる
* 5種類の評価関数<br>
各評価関数は以下のように評価をする:
 1. 石数
 2. 相手の打てる場所の数
 3. 46手目までは自分の打てる場所の数-相手の打てる場所の数、それ以降は石の数
 4. 46手目までは自分の打てる場所の数-相手の打てる場所の数、それ以降は石の数(打てる場所は重みがついている)
 5. 打てる場所の重み付き和の差+重み付き石数の差
 
 デフォルトではiv.を使う。オプション`-s`または`--heuristics`で変更できる。
* 接待モード<br>
 オプション`-l`または`--losing`を付けるとわざと負けようとする。
* 人間用<br>
 オプション`-m`または`--human`を付けると人間用の操作画面を表示する。

