<?php 

session_start();
if (!isset($_SESSION["i"])) {
    $_SESSION["i"] = 0;
}

if (!isset($_SESSION["unicodes"])){
    $_SESSION["unicodes"] = array("···· · ·–·· ·–·· –––  ·–– ––– ·–· ·–·· –··", 
        "––– ····  –– –·––  ––· ––– –··  ·· –  ·· ···  ··– –· ·· –·–· ––– –·· ·", 
        "–·· –––  –· ––– –  ·––· ·– –· ·· –·–·",
        "–– ––– ·–· ·  ·· –· ··· – ·–· ··– –·–· – ·· ––– –· ···  ·· –·  – ···· ·  ·– –· ··· ·–– · ·–· ···  –··· · ·–·· ––– ·––",
        "··  ···· ––– ·––· ·  –·–– ––– ··–  ·– ·–· ·  ···· ·– ···– ·· –· ––·  ··–· ··– –·  ·–– ·· – ····  –– ––– ·–· ··· ·  –·–· ––– –·· ·",
        "–·· ·· –··  –·–– ––– ··–  –· ––– – ·· –·–· ·  – ···· ·  – ·–– –––  ··· ·––· ·– –·–· · ···  –··· · – ·–– · · –·  ·–– ––– ·–· –·· ···  ·· –·  ···· – –– ·–··  ··· ––– ··– ·–· –·–· ·  ––·– ··– · ··· – ·· ––– –·  –– ·– ·–· –·–",
        "–– ––– ·–· ··· ·  –·–· ––– –·· ·  ·· ···  ·–  –– · – ···· ––– –··  ––– ··–·  – ·–· ·– –· ··· –– ·· – – ·· –· ––·  – · –··– –  ·· –· ··–· ––– ·–· –– ·– – ·· ––– –·  ·– ···  ·–  ··· · ·–· ·· · ···  ––– ··–·  ––– –·  ––– ··–· ··–·  – ––– –· · ···  ·–·· ·· ––· ···· – ···  ––– ·–·  –·–· ·–·· ·· –·–· –·– ···  – ···· ·– –  –·–· ·– –·  –··· ·  –·· ·· ·–· · –·–· – ·–·· –·––  ··– –· –·· · ·–· ··· – ––– ––– –··  –··· –·––  ·–  ··· –·– ·· ·–·· ·–·· · –··  ·–·· ·· ··· – · –· · ·–·  ––– ·–·  ––– –··· ··· · ·–· ···– · ·–·  ·–– ·· – ···· ––– ··– –  ··· ·––· · –·–· ·· ·– ·–··  · ––·– ··– ·· ·––· –– · –· –",
        "···· · ·–· ·  ·– ·–· ·  – ···· ·  ·–· ··– ·–·· · ···  ··–· ––– ·–·  – ···· ·· ···  ––· ·– –– ·  ··· – ––– ·––·  –·–– ––– ··– ·–·  – · –··– –  ··· ···· ––– ··– ·–·· –··  –·–· ––– –· – ·– ·· –·  ·––– ··– ··· –  ·–·· · – – · ·–· ···  ·– –· –··  ··· ·––· ·– –·–· · ···  ··· – ––– ·––·  ·–·· ––– ·–– · ·–· –·–· ·– ··· ·  ·– –· –··  ··– ·––· ·––· · ·–· –·–· ·– ··· ·  ·– ·–· ·  –·–· ––– –· ··· ·· –·· · ·–· · –··  – ···· ·  ··· ·– –– ·  ··· – ––– ·––·  ·· –·  –·–– ––– ··– ·–·  –– ––– ·–· ··· ·  –·–· ––– –·· ·  –·–– ––– ··–  ··· ···· ––– ··– ·–·· –··  ···· ·– ···– ·  ––– –· ·  ··· ·––· ·– –·–· ·  –··· · – ·–– · · –·  ·–·· · – – · ·–· ···  ·– –· –··  – ·–– –––  ··· ·––· ·– –·–· · ···  –··· · – ·–– · · –·  ·–– ––– ·–· –·· ···  ··· – ––– ·––·  ···· ·– ···– ·  ··–· ··– –·  ··· – ––– ·––·  ––– ···– · ·–·  ·– –· –··  ––– ··– –",
        "··  –– ·– –·––  –· ––– –  ···· ·– ···– ·  ––· ––– –· ·  ·–– ···· · ·–· ·  ··  ·· –· – · –· –·· · –··  – –––  ––· –––  –··· ··– –  ··  – ···· ·· –· –·–  ··  ···· ·– ···– ·  · –· –·· · –··  ··– ·––·  ·–– ···· · ·–· ·  ··  –· · · –·· · –··  – –––  –··· ·",
        "·· –·  – ···· ·  –··· · ––· ·· –· –· ·· –· ––·  – ···· ·  ··– –· ·· ···– · ·–· ··· ·  ·–– ·– ···  –·–· ·–· · ·– – · –··  ··· – ––– ·––·  – ···· ·· ···  ···· ·– ···  –– ·– –·· ·  ·–  ·–·· ––– –  ––– ··–·  ·––· · ––– ·––· ·–·· ·  ···– · ·–· –·––  ·– –· ––· ·–· –·––  ·– –· –··  –··· · · –·  ·–– ·· –·· · ·–·· –·––  ·–· · ––· ·– ·–· –·· · –··  ·– ···  ·–  –··· ·– –··  –– ––– ···– ·  ··· – ––– ·––·",
        "– ···· · ·–· ·  ·· ···  ·–  – ···· · ––– ·–· –·––  ·–– ···· ·· –·–· ····  ··· – ·– – · ···  – ···· ·– –  ·· ··–·  · ···– · ·–·  ·– –· –·–– ––– –· ·  –·· ·· ··· –·–· ––– ···– · ·–· ···  · –··– ·– –·–· – ·–·· –·––  ·–– ···· ·– –  – ···· ·  ··– –· ·· ···– · ·–· ··· ·  ·· ···  ··–· ––– ·–·  ·– –· –··  ·–– ···· –·––  ·· –  ·· ···  ···· · ·–· ·  ·· –  ·–– ·· ·–·· ·–··  ·· –· ··· – ·– –· – ·–·· –·––  –·· ·· ··· ·– ·––· ·––· · ·– ·–·  ·– –· –··  –··· ·  ·–· · ·––· ·–·· ·– –·–· · –··  –··· –·––  ··· ––– –– · – ···· ·· –· ––·  · ···– · –·  –– ––– ·–· ·  –··· ·· ––·· ·– ·–· ·–· ·  ·– –· –··  ·· –· · –··– ·––· ·–·· ·· –·–· ·– –··· ·–·· ·  ··· – ––– ·––·  – ···· · ·–· ·  ·· ···  ·– –· ––– – ···· · ·–·  – ···· · ––– ·–· –·––  ·–– ···· ·· –·–· ····  ··· – ·– – · ···  – ···· ·– –  – ···· ·· ···  ···· ·– ···  ·– ·–·· ·–· · ·– –·· –·––  ···· ·– ·––· ·––· · –· · –··",
        "·· –  ·· ···  –·– –· ––– ·–– –·  – ···· ·– –  – ···· · ·–· ·  ·– ·–· ·  ·– –·  ·· –· ··–· ·· –· ·· – ·  –· ··– –– –··· · ·–·  ––– ··–·  ·–– ––– ·–· ·–·· –·· ···  ··· ·· –– ·––· ·–·· –·––  –··· · –·–· ·– ··– ··· ·  – ···· · ·–· ·  ·· ···  ·– –·  ·· –· ··–· ·· –· ·· – ·  ·– –– ––– ··– –· –  ––– ··–·  ··· ·––· ·– –·–· ·  ··–· ––– ·–·  – ···· · ––  – –––  –··· ·  ·· –·  ··· – ––– ·––·  ···· ––– ·–– · ···– · ·–·  –· ––– –  · ···– · ·–· –·––  ––– –· ·  ––– ··–·  – ···· · ––  ·· ···  ·· –· ···· ·– –··· ·· – · –··  ··· – ––– ·––·  – ···· · ·–· · ··–· ––– ·–· ·  – ···· · ·–· ·  –– ··– ··· –  –··· ·  ·–  ··–· ·· –· ·· – ·  –· ··– –– –··· · ·–·  ––– ··–·  ·· –· ···· ·– –··· ·· – · –··  ·–– ––– ·–· ·–·· –·· ···  ··· – ––– ·––·  ·– –· –·––  ··–· ·· –· ·· – ·  –· ··– –– –··· · ·–·  –·· ·· ···– ·· –·· · –··  –··· –·––  ·· –· ··–· ·· –· ·· – –·––  ·· ···  ·– ···  –· · ·– ·–·  – –––  –· ––– – ···· ·· –· ––·  ·– ···  –– ·– –·– · ···  –· –––  ––– –·· –·· ···  ··· –––  – ···· ·  ·– ···– · ·–· ·– ––· ·  ·––· ––– ·––· ··– ·–·· ·– – ·· ––– –·  ––– ··–·  ·– ·–·· ·–··  – ···· ·  ·––· ·–·· ·– –· · – ···  ·· –·  – ···· ·  ··– –· ·· ···– · ·–· ··· ·  –·–· ·– –·  –··· ·  ··· ·– ·· –··  – –––  –··· ·  ––·· · ·–· –––  ··· – ––– ·––·  ··–· ·–· ––– ––  – ···· ·· ···  ·· –  ··–· ––– ·–·· ·–·· ––– ·–– ···  – ···· ·– –  – ···· ·  ·––· ––– ·––· ··– ·–·· ·– – ·· ––– –·  ––– ··–·  – ···· ·  ·–– ···· ––– ·–·· ·  ··– –· ·· ···– · ·–· ··· ·  ·· ···  ·– ·–·· ··· –––  ––·· · ·–· –––  ·– –· –··  – ···· ·– –  ·– –· –·––  ·––· · ––– ·––· ·–·· ·  –·–– ––– ··–  –– ·– –·––  –– · · –  ··–· ·–· ––– ––  – ·· –– ·  – –––  – ·· –– ·  ·– ·–· ·  –– · ·–· · ·–·· –·––  – ···· ·  ·––· ·–· ––– –·· ··– –·–· – ···  ––– ··–·  ·–  –·· · ·–· ·– –· ––· · –··  ·· –– ·– ––· ·· –· ·– – ·· ––– –·"
    );
}

if (!isset($_SESSION["answers"])){
    $_SESSION["answers"] = array(
        "HELLO WORLD", 
        "OH MY GOD IT IS UNICODE",
        "DO NOT PANIC",
        "MORE INSTRUCTIONS IN THE ANSWERS BELOW",
        "I HOPE YOU ARE HAVING FUN WITH MORSE CODE",
        "DID YOU NOTICE THE TWO SPACES BETWEEN WORDS IN HTML SOURCE QUESTION MARK",
        "MORSE CODE IS A METHOD OF TRANSMITTING TEXT INFORMATION AS A SERIES OF ON OFF TONES LIGHTS OR CLICKS THAT CAN BE DIRECTLY UNDERSTOOD BY A SKILLED LISTENER OR OBSERVER WITHOUT SPECIAL EQUIPMENT",
        "HERE ARE THE RULES FOR THIS GAME STOP YOUR TEXT SHOULD CONTAIN JUST LETTERS AND SPACES STOP LOWERCASE AND UPPERCASE ARE CONSIDERED THE SAME STOP IN YOUR MORSE CODE YOU SHOULD HAVE ONE SPACE BETWEEN LETTERS AND TWO SPACES BETWEEN WORDS STOP HAVE FUN STOP OVER AND OUT",
        "I MAY NOT HAVE GONE WHERE I INTENDED TO GO BUT I THINK I HAVE ENDED UP WHERE I NEEDED TO BE",
        "IN THE BEGINNING THE UNIVERSE WAS CREATED STOP THIS HAS MADE A LOT OF PEOPLE VERY ANGRY AND BEEN WIDELY REGARDED AS A BAD MOVE STOP",
        "THERE IS A THEORY WHICH STATES THAT IF EVER ANYONE DISCOVERS EXACTLY WHAT THE UNIVERSE IS FOR AND WHY IT IS HERE IT WILL INSTANTLY DISAPPEAR AND BE REPLACED BY SOMETHING EVEN MORE BIZARRE AND INEXPLICABLE STOP THERE IS ANOTHER THEORY WHICH STATES THAT THIS HAS ALREADY HAPPENED",
        "IT IS KNOWN THAT THERE ARE AN INFINITE NUMBER OF WORLDS SIMPLY BECAUSE THERE IS AN INFINITE AMOUNT OF SPACE FOR THEM TO BE IN STOP HOWEVER NOT EVERY ONE OF THEM IS INHABITED STOP THEREFORE THERE MUST BE A FINITE NUMBER OF INHABITED WORLDS STOP ANY FINITE NUMBER DIVIDED BY INFINITY IS AS NEAR TO NOTHING AS MAKES NO ODDS SO THE AVERAGE POPULATION OF ALL THE PLANETS IN THE UNIVERSE CAN BE SAID TO BE ZERO STOP FROM THIS IT FOLLOWS THAT THE POPULATION OF THE WHOLE UNIVERSE IS ALSO ZERO AND THAT ANY PEOPLE YOU MAY MEET FROM TIME TO TIME ARE MERELY THE PRODUCTS OF A DERANGED IMAGINATION"
    );
}

?>

<!DOCTYPE html>
<html>
    <head>
    <meta http-equiv="Content-Type" content="text/html; charset=UTF-8">
    <title>Yet another simple code game!</title>
    <style type="text/css">
    <!--
    body,td,th {
    font-family: Verdana, Arial, Helvetica, sans-serif;
    font-size: x-large;
    color: #CCCCCC;
    }

    body {
    background-color: #333399;
    }

    .title {
    font-family: "Courier New", Courier, monospace;
    font-weight: bold;
    font-size: 48px;
    color: #00FF66;
    }

    .question {color: #FFCC33}
    .number {color: #FFFF33}
    .md5sum {color: #FFCCFF}
    .emph {color: #99ee99}
    .alert {color: #ee77aa}

    .right {
    color: #33FF66;
    font-weight: bold;
    }
    .wrong {
    color: #FF3366;
    font-weight: bold;
    }

    a:link {
    color: #CCFFFF;
    }

    a:visited {
    color: #CCFFFF;
    }

    input {
    background-color: #eeee66;
    color: #333399;
    }

    code {
    text-wrap: lowercase;   
    font-family: monospace;
    display: block;
    background-color: #66eeee;
    color: #993333;
    border: 1px solid black;
    padding: 8px;
    width: 95%;
    margin-bottom: 2em;
    }

    textarea.wide {
    text-wrap: lowercase;
    font-family: monospace;
    font-size: x-large;
    color: #333333;
    border: 1px solid black;
    padding: 8px;
    width: 95%;
    }

    -->
    </style>
    </head>
    <body>


    <h1>Yet another simple code game!</h1>
    <p><span class="question">Question <?php echo $_SESSION["i"]+1; ?> </span>:</p>
    <code><?php echo html_entity_decode($_SESSION["unicodes"][$_SESSION["i"]]); ?></code>
    
    <?php 
    if ($_SERVER['REQUEST_METHOD'] === 'POST' && isset($_POST["answer"])) {
        if ($_POST["answer"] === $_SESSION["answers"][$_SESSION["i"]]) {
    ?>
            <p class="right">Right!  :-)</p>
            <hr>
    <?php
            if ($_SESSION["i"] == 11) {
                $_SESSION["i"] = 0;
    ?>
                <form method="post">
                    <input type="hidden" id="reset" name="reset" value="reset">
                    <input type="submit" name="again" id="again" value="Play again!">
                </form>
    <?php
            }
            else {
                $_SESSION["i"] = $_SESSION["i"]+1; 
    ?>
                <form method="post">
                    <input type="hidden" id="continue" name="continue" value="continue">
                    <input type="submit" name="again" id="again" value="Continue!">
                </form>
    <?php        
            }

        } else {
    ?>      
            <p class="wrong">Wrong!  :-(</p>
            <hr>
            <form method="post">
                <input type="hidden" id="continue" name="continue" value="continue">
                <input type="submit" name="again" id="again" value="Play again!">
            </form>
    <?php
        } 
        unset($_POST['answer']); 
    } else { 
    ?>  
        <span class="question">Make it quick, the clock is ticking...</span><p></p>
        <form method="post">
            <textarea class="wide" name="answer" id="answer"></textarea>
            <input type="submit" name="submit" id="submit" value="Submit!">
            <input type="reset" value="Reset">
        </form>
    <?php } ?>    
    </body>
</html>