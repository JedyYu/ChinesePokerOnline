# ChinesePoker
欢乐斗地主

使用：
1、开启服务器：
    ./server/pokerserver
2、开启客户端：
    ./client/hello

游戏流程（客户端）：
1、输入主机IP
2、输入玩家昵称
3、开始游戏：抢地主、。。。

客户端文件结构：
1、主文件：hello.hs
2、出牌逻辑函数：card.hs

服务器文件结构：
1、主文件：pokerserver.hs
2、定义牌的数据结构：cards.hs
3、定义洗牌和发牌函数：deck.hs
4、定义Player和Game两种数据结构：player.hs
