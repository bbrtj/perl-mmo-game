use 5.030001;
use ExtUtils::MakeMaker;

WriteMakefile(
	NAME => 'Game::Unit',
	VERSION_FROM => 'lib/Game/Unit.pm',
	PREREQ_PM => {
		'Game::Common' => 0,
		'Game::Schema' => 0,
	},
	AUTHOR => 'brtastic',
);
