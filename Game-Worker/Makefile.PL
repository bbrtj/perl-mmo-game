use 5.030001;
use ExtUtils::MakeMaker;

WriteMakefile(
	NAME => 'Game::Worker',
	VERSION_FROM => 'lib/Game/Worker.pm',
	PREREQ_PM => {
		'Game::Common' => 0,
		'Minion' => 0,
	},
	AUTHOR => 'brtech',
);
