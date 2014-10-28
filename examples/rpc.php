<?
    $url = 'http://v2r.me/rpc/auth';
    $domain = "test";
    $client_key = "tJ2YvnmsPpYJn7fwG9qqhpDmMG891m1z";
    $server_key = "x9eIt0IlKnt3PeGR3DPps2TGQnRHhtD5";

    $u = array();
    if($_POST['mail'] && $_POST['password']) {
        $mail = $_POST['mail'];
        $md5pwd = md5($_POST['password']);
        $sign = md5($mail . $md5pwd . $client_key);

        $data = array('mail' => $mail, 'password' => $md5pwd, 
            "sign" => $sign, "domain" => $domain);
        $options = array(
            'http' => array(
                'header'  => "Content-type: application/x-www-form-urlencoded\r\n",
                'method'  => 'POST',
                'content' => http_build_query($data),
            ),
        );
        $context  = stream_context_create($options);
        $u = json_decode(file_get_contents($url, false, $context), true);
        
        if($u['sign']) {
            if(md5($u['result'] . ($u['result'] == 'ok' ? $u['mail'] . $u['name'] : $u['error']) . $server_key) != $u['sign']) 
                $u = array();
        }
    }
?>
<!doctype html>
<html lang="ru">
  <head>
    <meta charset="UTF-8">
  </head>
  <body>
    <? if($u['result'] != 'ok') { 
        if($u['result'] == 'error' && $u['error'] == 'not_found') { ?> 
            Not found, try one more time... <br/><br/>
        <? }; ?>
      <form action="rpc.php" method="post">
        @: <input type="text" name="mail"/><br/>
        .: <input type="password" name="password"/><br/>
        <input type="submit">
      </form>
      <br/><br/>
      <a href="https://v2r.me/#reg">Registration</a> 
      <a href="https://v2r.me/#reset">Password reset</a>
    <? } else { ?>
      <h1>Wellcome, <?= $u['name'] ?>!</h1>
    <? } ?>
  </body>
</html>