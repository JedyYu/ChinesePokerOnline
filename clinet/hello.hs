module Main where
-- for game logic
import Cards
-- for Socket
import Network.Socket hiding (close)
import System.IO
import Control.Exception
import Control.Concurrent
import Control.Concurrent.Chan
import Control.Monad
import Data.Word
import Control.Concurrent.MVar
import Network.BSD
import Data.Bits
import Control.Monad.Fix (fix)
-- for GUI
import Graphics.UI.WX
import System.Random

-- settings
clo = 0.01 ::Float
tim = 100.0 ::Float
--iP = "175.186.105.201"
port = "4242"

-- type of messages and transmitting function
type Msg = (Int, String)
type HandlerFunc =  StaticText () ->  StaticText () ->  Var String -> [BitmapButton ()]-> 
         [StaticText ()] -> [BitmapButton ()]-> [BitmapButton ()]-> [BitmapButton ()]->
         [BitmapButton ()]-> [BitmapButton ()]-> [Button ()]-> [Button ()]-> Panel () -> 
         Panel () -> Panel () -> Panel () -> Panel () -> Panel () ->Frame () ->StaticText ()->
         Socket->AddrInfo -> Var Float -> Var [[Char]] -> Var [Char] -> Var [Char] -> 
         StaticText () ->  StaticText () ->  StaticText () -> String -> IO ()


--default
startc = "2"
defaultmessage = "left player;2H,2H,2H,2H,2H,2H,2H,2H,2H,2H,2H,2H,2H,2H,2H,2H,2H,2H,2H,2H,;;1;1;0;myname;;;1;1;0;right player;2H,2H,2H,2H,2H,2H,2H,2H,2H,2H,2H,2H,2H,2H,2H,2H,2H,2H,2H,2H,;;1;1;0;myname;;;2H,2H,2H,;???;???;0;1;"

--sounds
s0 = sound ("./media/s0_01.wav")
buyao = sound ("./media/buyao.wav")
dani = sound ("./media/dani.wav")
qdz = sound ("./media/qdz.wav")
dzswdl = sound ("./media/dizhushiwol.wav")
wangzha = sound ("./media/wangzha.wav")

-- main function
main = hello plainHandler

-- main function with a input variable
hello ::HandlerFunc-> IO ()
hello handlerfunc  
  = do
    -- get IP
       putStrLn "Input server IP:"
       iP <- getLine
    -- start the connection
       sock <- socket AF_INET Stream defaultProtocol
       setSocketOption sock KeepAlive 1
       addrinfos <- getAddrInfo  Nothing (Just iP) (Just port)
       let serveraddr = head addrinfos
       connect sock (addrAddress serveraddr)
    -- get message
       hdl <- socketToHandle sock ReadMode
       hSetBuffering hdl LineBuffering
       messages <- hGetContents hdl
    -- do something with messages
       start (gui sock serveraddr handlerfunc defaultmessage messages)
       return ()

-- GUI
gui sock serveraddr handlerfunc defaultmessage messages
  = do 
-- set variables
       sele <- varCreate []
       ipp <- varCreate []
       roundcount <- varCreate "0"
       tick <- varCreate tim
       mname <- varCreate "myname"

-- starting sounds
       play s0
-- main frame fixed
       f <- frameFixed [text:="欢乐斗地主", outerSize := sz 1024 600, bgcolor:=rgb 256 100 27
                       , position := pt 250 100]
       out <- staticText f [text:= "myname", position:=pt 20 550, fontSize:= 20]

-- user name frame
       ff <- frame [text:="新游戏", position:= pt 550 330] 
       namein <- entry ff [text:="myname"]
       ok <- button ff [text:="确定", on command:= do s <- get namein text;
                                                      givename out sock serveraddr s tick;
                                                      varUpdate mname (\x->s);
                                                      set out [text:=s];
                                                      stop s0;
                                                      close ff]
       set ff [layout:= floatCentre $ margin 30 $ column 10 [widget namein
                                                            ,widget ok]
              , outerSize:= sz 400 225]
        
-- set default state
       myname <- varGet mname
       let      msg = cutstring defaultmessage ';'
                iname
                     | (msg!!0==myname) = 0
                     | (msg!!6==myname) = 6
                     |otherwise = 12
                player0 = cutstring (msg!!(1 + iname)) ','
                playerr = cutstring (msg!!(1 + mod (iname+6) 18)) ','
                playerl = cutstring (msg!!(1 + mod (iname+12) 18)) ','
                wildcard = cutstring (msg!!21) ','
                l2 = cutstring (msg!!19) ','
                l1 = cutstring (msg!!20) ','
       varSet ipp (msg!!18)
       ip <- varGet ipp
       let      dealcardm = cutstring (msg!!(2 + iname)) ','
                dealcardr = cutstring (msg!!(2 + mod (iname+6) 18)) ','
                dealcardl = cutstring (msg!!(2+ mod (iname+12) 18)) ','
                namer = msg!!(0 + mod (iname+6) 18)
                namel = msg!!(0 + mod (iname+12) 18)
       rc <- varGet roundcount
       if rc /= msg!!25 then do varSet roundcount rc
                        else return ()

-- head panel
    -- show the state of game
       pstate <- panel f [clientSize:= sz 350 130, position:= pt 0 0]     
       tstate <- showstate pstate
       set (tstate!!0) [position:= pt 10 10]
       set (tstate!!1) [position:= pt 10 50]
        -- method to change
       --if not qdz then changestate (tstate!!0) (tstate!!1) nload times 
       --           else return ()
    -- wild card
       pwildcard <- panel f [clientSize:= sz 300 130, position:= pt 362 0]
       bwildcard <- getcardh wildcard pwildcard 0
       -- method to change
        --if not qdz then changewildcard bwildcard wildcard 
          --        else return ()

-- left player
       pleft <- panel f [clientSize:= sz 150 310, position:=pt 0 130]
       bleft <- getcardv playerl pleft playerl 0 0
       clearcard bleft 
       tleft <- staticText pleft [text:=namel, position:=pt 10 280, fontSize:= 18]
       let delta 
                |length playerl <8 = 15
                |otherwise = div 171 (length playerl - 1)
       tleftc <- staticText pleft [text:=(show (length playerl))
                                  , position:= pt 90 (delta * (length playerl - 1)+50)
                                  , fontSize:= 40, textColor:=blue]
    -- method to change it
            -- do changev bleft playerl pleft 0 0                                   
            --      let delta 
            --                    |length playerl <8 = 15
            --                    |otherwise = div 171 (length playerl - 1)
            --      set tleftc [text:=(show (length playerl)), position:= pt 90 (delta * (length playerl - 1)+50)] 

-- right player
       pright <- panel f [clientSize:=sz 150 310, position:=pt 874 130]
       bright <- getcardv playerr pright playerr 63 0
       clearcard bright
       let delta 
                |length playerr <8 = 15
                |otherwise = div 171 (length playerr - 1)
       tright <- staticText pright [text:=namer, position:=pt 35 280, fontSize:= 18]
       trightc <- staticText pright [text:=(show (length playerr))
                                    , position:= pt 27 (delta * (length playerr - 1)+50)
                                    , fontSize:= 40, textColor:= blue]
    -- method to change it
            -- do changev bright playerr pright 63 0       
            --        let delta                                                   
            --                 |length playerr <8 = 15
            --                 |otherwise = div 171 (length playerr - 1)
            --    set trightc [text:=(show (length playerr)), position:= pt 27 (delta * (length playerr - 1)+50)]

-- center part
       pcentre <- panel f [clientSize:=sz 700 270, position:=pt 162 130]
       bshowleft <- getcardm dealcardl pcentre dealcardl namel 0 namel namer myname 
       bshowright <- getcardm dealcardr pcentre dealcardr namer 0 namel namer myname 
       bshowme <- getcardm dealcardm pcentre dealcardm myname 0 namel namer myname
    -- method to change it          
            -- do changecenter bshowleft dealcardl pcentre "l"                    
            --    changecenter bshowright dealcardr pcentre "r"
            --    changecenter bshowme dealcardm pcentre "m"
            --    return ()

-- command buttons
       cmdsc <- panel f [clientSize:= sz 700 30, position:=pt 162 405]
       cmds <- panel f [clientSize:= sz 700 30, position:=pt 162 405, bgcolor:=rgb 256 100 27] 
    --button of qdz(default)
       bconc <- getbconc cmdsc ip myname out sele sock serveraddr
       set (bconc!!1) [enabled:=False]
       bcon <- getbcon cmds ip myname out sock serveraddr
    -- method to change it
       --if qdz then refreshbcon bcon cmds ip canq out
       --       else changebcon bcon cmds ip canc out sele

-- main player
    -- card list
       pcard <- panel f [clientSize:= sz 700 160, position:=pt 162 430 ]
       bcard <- getbutcard player0 pcard f player0 0 sele l1 l2 bconc True
    -- method to change it 
        -- changemancard player0 pcard f sele

-- clock
       clockm <- staticText f [text:=(show (round tim)), position:= pt 900 500
                              , fontSize:=40, textColor:=red]
       tc <- timer f [interval:=round (clo*1000)
                     , on command:= do ticks <- varGet tick
                                       if ticks > 0 
                                           then do varUpdate tick (\x->x-clo)
                                                   set clockm [text:=show (round (ticks - clo))]
                                                   ip <- varGet ipp
                                                   if (ip==myname)&&(ticks<=clo) 
                                                       then do pass out sock serveraddr "0" myname
                                                       else return ()
                                           else return ()]
    -- method to change it 
        -- setclock f ip namel namer 

-- show time   
       ptimetable <- panel f [clientSize:= sz 350 130, position:= pt 674 0]
             --q <- button ptimetable [text:="斗地主"
       --                       ,on command := do changestate (tstate!!0) (tstate!!1) nload times
       --                                         changewildcard bwildcard wildcard
       --                                         return ()]
       
-- handle message
       lock <- newMVar ()
       let handles :: MVar () -> HandlerFunc
           handles lock tleft tright mname bwildcard tstate bleft bright bshowme 
               bshowleft bshowright bcon bconc cmdsc pleft 
               pright pcentre pcard cmds f out sock serveraddr tick sele ipp 
               roundcount tleftc trightc clockm messages = 
               withMVar lock
                   (\a -> handlerfunc tleft tright mname bwildcard tstate bleft 
                       bright bshowme bshowleft bshowright 
                       bcon bconc cmdsc pleft pright pcentre pcard cmds f out sock serveraddr 
                           tick sele ipp roundcount tleftc trightc clockm messages 
                           >> return a) 
       forkIO $ mapM_ (handles lock tleft tright mname bwildcard tstate bleft bright 
                    bshowme bshowleft bshowright bcon bconc
                    cmdsc pleft pright pcentre pcard cmds f out sock serveraddr tick sele 
                    ipp roundcount tleftc trightc clockm) (lines messages)

       return ()
  
-- state
    -- show state
showstate pstate
  = do tnload <- staticText pstate [text:="地主：???", fontWeight:=WeightBold
                                   , fontSize:=20, fontFamily:=FontScript]
       ttimes <- staticText pstate [text:="倍数：???", fontWeight:=WeightBold
                                   , fontSize:=20]
       return [tnload,ttimes]
    -- change state
changestate tnload ttimes nload times
  = do set tnload [text:="地主： "++nload]
       set ttimes [text:="倍数： "++times]

-- left and right
    -- cardlist vertical
getcardv [] pan playerc px py = do return []
getcardv (x:xs) pan playerc px py
  = do let delta 
                |length playerc <8 = 15
                |otherwise = div 171 (length playerc - 1)
       cardi <-bitmapButton  pan [picture:=("./pic/backjpd.jpg")
                                 , position:=pt px py, style:=50]
       a <- getcardv xs pan playerc px (py+delta)
       return ([cardi]++a)
    -- change vertical cardlist
changev b playerc pan px py 
  = do clearcard b
       changecardv playerc b playerc px py
       return ()
    -- clearcard
clearcard [] = return ()
clearcard (b:bs) 
  = do set b [visible:= False]
       clearcard (bs)
       return ()
    -- change cardv
changecardv [] b playerc px py =return ()
changecardv (x:xs) (b:bx) playerc px py
  = do let delta 
                |length playerc <8 = 15
                |otherwise = div 171 (length playerc - 1)
       set b [visible:= True, position:=pt px py]
       changecardv xs bx playerc px (py+delta)
       return ()

-- wild card
    -- cardlist horizontal 
getcardh [] pan i= do return []
getcardh (x:xs) pan i
  = do cardi <-bitmapButton pan [picture:=("./pic/backjpd.jpg")
                                , position:=pt (i+15) 0, style:=100]
       a <- getcardh xs pan (i+92)
       return ([cardi]++a)
    -- change wild card
changewildcard [] [] = do return ()
changewildcard (b:bs) (x:xs)
  = do set b [picture:=("./pic/"++x++".jpg")]
       changewildcard bs xs 
       return ()

-- central part
    -- cardlist dealed
getcardm [] pan [] pl i namel namer myname
  = do let px
             |pl==myname = 250
             |pl==namel = 0
             |otherwise = 500
           py
             |pl==myname = 140
             |otherwise = 10
       cardi <- bitmapButton pan [picture:="./pic/no.jpg", position:=pt px py]
       return [cardi]
getcardm [] pan dealcard pl i namel namer myname= do return []
getcardm (x:xs) pan dealcard pl i namel namer myname
  = do let px
             |pl==myname = (div (700-(length dealcard)*20-67) 2+i) 
             |pl==namel = 0+i
             |otherwise = 700-(length dealcard)*20-67+i
           py
             |pl==myname = 140
             |otherwise = 10
       cardi <-bitmapButton pan [picture:=("./pic/"++x++".jpg"), position:=pt px py
                                , style:=100]
       a <- getcardm xs pan dealcard pl (i+20) namel namer myname 
       return ([cardi]++a)
    -- change centre
changecenter b dealcard pan pl namel namer myname
  = do clearcard b
       if pl==myname 
           then do bshowme <- getcardm dealcard pan dealcard pl 0 namel namer myname 
                   return ()
           else if pl==namel 
                    then do bshowleft <- getcardm dealcard pan dealcard pl 0 namel namer myname
                            return ()
                    else do bshowright <- getcardm dealcard pan dealcard pl 0 namel namer myname 
                            return ()
       return ()

-- main player
    -- cardlist
getbutcard [] pan fr playerc i sele l1 l2 bconc ipmy= do return []
getbutcard (x:xs) pan fr playerc i sele l1 l2 bconc ipmy
  = do ss <- varGet sele
       let hight x ss
                     |elem x ss = 0
                     |otherwise =17
           px=(div (700-(length playerc)*20-67) 2+i) 
       cardi <- bitmapButton pan 
           [picture:=("./pic/"++x++".jpg")
           ,position:=pt px (hight x ss)
           ,on command:= do if (not (elem x ss)) 
                                then varUpdate sele ([x]++)
                                else varUpdate sele ((\x ss->[s|s<-ss,s/=x]) x)
                            ss <- varGet sele
                            putStrLn (show ss)
                            putStrLn (show (validPlay1 l1 l2 ss))
                            set (bconc!!1) [enabled:=(validPlay1 l1 l2 ss)&&(ipmy)]
                            set pan [visible:= False]
                            card <- panel fr [clientSize:= sz 700 160, bgcolor:=rgb 256 100 27, position:=pt 162 430 ]
                            getbutcard playerc card fr playerc 0 sele l1 l2 bconc ipmy
                            return ()
           ,style:=100]
       a <- getbutcard xs pan fr playerc (i+20) sele l1 l2 bconc ipmy
       return ([cardi]++a)
    -- change cardlist
changemancard playerc pan fr sele l1 l2 bconc ipmy
        = do set pan [visible:= False]
             card <- panel fr [clientSize:= sz 700 160, bgcolor:=rgb 256 100 27
                              , position:=pt 162 430 ]
             bcard <- getbutcard playerc card fr playerc 0 sele l1 l2 bconc ipmy
             return ()

-- control
    -- control button
getbcon pan ip myname out sock serveraddr
   = do butq0 <- button pan [text:="不抢", position:=pt 150 0]
        butq1 <- button pan [text:="一倍", position:= pt 250 0]
        butq2 <- button pan [text:="两倍", position:= pt 350 0]
        butq3 <- button pan [text:="三倍", position:= pt 450 0]
        set butq0 [on command := do pass out sock serveraddr "0" myname ;set butq1 [visible:= False]
                                    set butq2 [visible:= False];set butq3 [visible:= False]
                                    set butq0 [enabled:= False];play (sound "./media/buyao.wav")]
        set butq1 [on command := do pass out sock serveraddr "1" myname ;set butq0 [visible:= False]
                                    set butq2 [visible:= False];set butq3 [visible:= False]
                                    set butq1 [enabled:= False]]
        set butq2 [on command := do pass out sock serveraddr "2" myname ;set butq0 [visible:= False]
                                    set butq1 [visible:= False];set butq3 [visible:= False]
                                    set butq2 [enabled:= False]]
        set butq3 [on command := do pass out sock serveraddr "3" myname ;set butq0 [visible:= False]
                                    set butq1 [visible:= False];set butq2 [visible:= False]
                                    set butq3 [enabled:= False]]
        return [butq0,butq1,butq2,butq3]
    -- update control of qdz
refreshbcon b pan ip myname out sele sock serveraddr
   = do clearcard b
        bconc <- getbconc pan ip myname out sele sock serveraddr
        return ()
    -- control button of deal
getbconc pan ip myname out sele sock serveraddr
   = do butc0 <- button pan [text:="不出", on command := pass out sock serveraddr "0" myname
                            , position:= pt 250 0, enabled:=ip==myname]
        butc1 <- button pan [text:="出牌", on command := deal out sele sock serveraddr myname
                            , position:= pt 350 0, enabled:=ip==myname]
        return [butc0,butc1]

    -- change to deal
changebcon b pan ip myname out sock serveraddr
        = do clearcard b
             bcon <- getbcon pan ip myname out sock serveraddr
             return ()
    -- function deal
deal out sele sock serveraddr myname
    =do ss<- varGet sele
        myname <- get out text
        varUpdate sele (\x->[])
        sendTo sock (myname++";"++(newconcat ss)++";0; \n") (addrAddress serveraddr)
        return ()

newconcat [] = ""
newconcat (x:xs) = x ++ "," ++ (newconcat xs)

    -- function pass
pass out sock serveraddr inf myname
   = do myname <- get out text
        sendTo sock (myname++";;"++inf++" \n") (addrAddress serveraddr)
        if inf=="0" then return ()
                    else play qdz
        return ()
    -- function nameout
givename out sock serveraddr username tick
    = do myname <- get out text
         varUpdate tick (\x->1000.0::Float)
         sendTo sock (username++";;0 \n") (addrAddress serveraddr)
         return ()

-- clock
setclock tick pl namel namer myname tim clockm
        = do let px
                   |pl==myname = 900
                   |pl==namel = 110
                   |otherwise = 865
                 py    
                   |pl==myname = 500
                   |otherwise = 130
             set clockm [position:=pt px py]
             varUpdate tick (\x->tim::Float)
             return ()

--cut string to list
cutstring [] m = []
cutstring s m = [fst (break (==m) s)] ++ cutstring (tail(snd (break (==m) s))) m

-- change function
plainHandler :: HandlerFunc
plainHandler tleft tright mname bwildcard tstate bleft bright bshowme bshowleft bshowright bcon bconc cmdsc pleft pright pcentre pcard cmds f out sock serveraddr tick sele ipp roundcount tleftc trightc clockm messages 
  = do if (length messages)>70 
         then do
           putStrLn messages
           myname <- get out text
           let
               getmessage = messages
               msg = cutstring getmessage ';'
               iname
                    | (msg!!0==myname) = 0
                    | (msg!!6==myname) = 6 
                    |otherwise = 12
               player0 = cutstring (msg!!(1 + iname)) ','
               playerr = cutstring (msg!!(1 + mod (iname+6) 18)) ','
               playerl = cutstring (msg!!(1 + mod (iname+12) 18)) ','
               wildcard = cutstring (msg!!21) ','
               l2 = cutstring (msg!!19) ','
               l1 = cutstring (msg!!20) ','
               l0 = cutstring (msg!!26) ','
           varUpdate ipp (\x->(msg!!18))
           ip <- varGet ipp
           let   
               dealcardm = cutstring (msg!!(2 + iname)) ','
               dealcardr = cutstring (msg!!(2 + mod (iname+6) 18)) ','
               dealcardl = cutstring (msg!!(2+ mod (iname+12) 18)) ','
               namer = msg!!(0 + mod (iname+6) 18)
               namel = msg!!(0 + mod (iname+12) 18)
               nload = msg!!22
               times = msg!!23
           rc <- varGet roundcount
           if (rc /= (msg!!25))
               then do varUpdate roundcount (\x->(msg!!25))
                       rc <- varGet roundcount
                       nn <- randomRIO (0,10) ::IO Int
                       let toplay 
                                 |(l2==[]) = buyao
                                 |(l2/=[])&&(l0==[])&&(l1==[]) = sound ("./media/c"++(show nn)++".mp3")
                                 |(l2==["Bj","Lj"])||(l2==["Lj","Bj"]) = wangzha
                                 |otherwise = dani
                       if not (ip==myname) then do set (bconc!!0) [enabled:=False]
                                                   set (bconc!!1) [enabled:=False]
                                                   return ()
                                           else do set (bconc!!0) [enabled:=True]
                                                   return ()
                       if rc=="1" then do set tleft [text:=namel]
                                          set tright [text:=namer]
                                          return ()
                                  else return ()
                       if rc==startc then do changestate (tstate!!0) (tstate!!1) nload times       
                                             set cmds [visible:=False]
                                             play dzswdl
                                             changewildcard bwildcard wildcard 
                                             return ()
                                     else return ()
                       if rc>startc then play toplay
                                    else return ()
                       if (length playerl)==0 || (length playerr==0) || (length player0==0) 
                           then do let m
                                        | (length playerl)==0 =namel
                                        | (length playerr)==0 =namer
                                        | otherwise = myname
                                   fff <- frame [text:="游戏结束", position:= pt 550 330] 
                                   win <- staticText fff [text:= m++" win！", fontSize:=40 ]
                                   ok <- button fff [text:="确定", on command:= do close fff;
                                                                                   close f;
                                                                                   return ()]
                                   set fff [layout:= floatCentre $ margin 30 $ column 10 [widget win
                                                                                         ,widget ok]
                                           , outerSize:= sz 400 225]
                                   return ()
                           else return ()
                       changev bleft playerl pleft 0 0
                       changev bright playerr pright 63 0
                       set pcentre [visible:=False]
                       pcentre <- panel f [clientSize:=sz 700 270, position:=pt 162 130, bgcolor:=rgb 256 100 27]
                       changecenter bshowleft dealcardl pcentre namel namel namer myname 
                       changecenter bshowme dealcardm pcentre myname namel namer myname 
                       changecenter bshowright dealcardr pcentre namer namel namer myname
                       changemancard player0 pcard f sele l1 l2 bconc (ip==myname)
                       let delta 
                                |length playerl <8 = 15
                                |otherwise = div 171 (length playerl - 1)
                       set tleftc [text:=(show (length playerl))
                                  , position:= pt 80 (delta * (length playerl - 1)+50)] 
                       let delta                                                      
                                 |length playerr <8 = 15
                                 |otherwise = div 171 (length playerr - 1)
                       set trightc [text:=(show (length playerr))
                                   , position:= pt 25 (delta * (length playerr - 1)+50)]
                       setclock tick ip namel namer myname tim clockm
               else return ()
           else return ()
