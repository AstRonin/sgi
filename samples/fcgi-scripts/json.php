<?php

$us = 'username';
$ps = 'password';

$u = $_POST[$us];
$p = $_POST[$ps];

echo json_encode([$us => $u, $ps => $p]);
