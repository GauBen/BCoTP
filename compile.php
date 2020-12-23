<?php

$main = './main.ml';
$bin = 'BigCitiesOnTinyPlanets.exe';
$cmd = 'ocamlopt -o "../bin/' . $bin . '"';
// $cmd = 'ocamlc -custom -o "./bin/' . $bin . '"'; // (cmxa -> cma)

echo 'Preparing .ml files...' . PHP_EOL;
$code = preg_replace_callback('/#(use|load) "(.+)";;/iU', function($matches) use(&$cmd) {
	if ($matches[1] == 'use') {
		return file_get_contents($matches[2]);
	} elseif ($matches[1] == 'load') {
		$cmd .= ' ' . str_replace('.cma', '.cmxa', $matches[2]);
	}
}, file_get_contents($main));
$cmd .= ' ../bin/bcotp.ml';

file_put_contents('../bin/bcotp.ml', $code);
echo 'Compiling... (`' . $cmd . '`)' . PHP_EOL;
exec($cmd, $output, $code);
if ($code === 0) {
	echo 'Success!!' . PHP_EOL;
	echo 'Run `./bin/' . $bin . '` to play' . PHP_EOL;
} else {
	echo 'Something went wront :(' . PHP_EOL;
	echo implode(PHP_EOL, $output);
}
