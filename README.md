<h1 align="center"> Caesar Cipher </h1>
<div align="center">
    <span align="center"> s i m p l e · h a s k e l l · i m p l e m e n t a t i o n</span>
</div>

<br />
<div align="center">
    <img src="/docs/img.png" align="center">
</div>
<br />

My goal with this project was to build a Caesar Cipher with the minimum effort. <br />
Why? Because I strongly believe that an awesome language such as Haskell, has the potential to transform the following verbose code in something beautiful:

```php
<?php
function encrypt($str, $offset) {
    $encrypted_text = "";
    $offset = $offset % 26;
    if($offset < 0) {
        $offset += 26;
    }
    $i = 0;
    while($i < strlen($str)) {
        $c = strtoupper($str{$i}); 
        if(($c >= "A") && ($c <= 'Z')) {
            if((ord($c) + $offset) > ord("Z")) {
                $encrypted_text .= chr(ord($c) + $offset - 26);
        } else {
            $encrypted_text .= chr(ord($c) + $offset);
        }
      } else {
          $encrypted_text .= " ";
      }
      $i++;
    }
    return $encrypted_text;
}

function decrypt($str, $offset) {
    $decrypted_text = "";
    $offset = $offset % 26;
    if($offset < 0) {
        $offset += 26;
    }
    $i = 0;
    while($i < strlen($str)) {
        $c = strtoupper($str{$i}); 
        if(($c >= "A") && ($c <= 'Z')) {
            if((ord($c) - $offset) < ord("A")) {
                $decrypted_text .= chr(ord($c) - $offset + 26);
        } else {
            $decrypted_text .= chr(ord($c) - $offset);
        }
      } else {
          $decrypted_text .= " ";
      }
      $i++;
    }
    return $decrypted_text;
}
```

why the `hwem` (caesar 2) should I write such an horrible php code when there's Haskell?

# License
[MIT](/LICENSE.md)