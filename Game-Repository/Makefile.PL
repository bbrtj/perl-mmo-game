use 5.030001;
use ExtUtils::MakeMaker;

WriteMakefile(
	NAME => 'Game::Repository',
	VERSION_FROM => 'lib/Game/Repository.pm',
	PREREQ_PM => {
		'Game::Common' => 0,
		'Game::Schema' => 0,
		'Game::Unit' => 0,
	},
	AUTHOR => 'brtastic',
);
