<?php

//echo json_encode($_SERVER);
//echo json_encode($_POST);
//echo json_encode($_GET);
//echo file_get_contents('php://input');

$us = 'username';
$ps = 'password';

$u = $_POST[$us];
$p = $_POST[$ps];

echo json_encode([$us => $u, $ps => $p]);
