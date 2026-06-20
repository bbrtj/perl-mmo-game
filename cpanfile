on 'runtime' => sub {
	requires 'Moose' => 0;
	requires 'Mooish::AttributeBuilder' => '1.001';
	requires 'Sub::HandlesVia' => 0;
	requires 'Hook::AfterRuntime' => 0;
	requires 'MooseX::XSAccessor' => 0;
	requires 'MooseX::StrictConstructor' => 0;

	requires 'Beam::Wire' => 0;

	requires 'Form::Tiny' => '2.06';
	requires 'Form::Tiny::Plugin::Diva' => 0;

	requires 'Type::Tiny' => 0;
	requires 'Type::Tiny::XS' => 0;
	requires 'Type::EmailAddress' => 0;
	requires 'Types::ULID' => 0;

	requires 'Thunderhorse' => '0.105';
	requires 'IO::Async' => 0;
	requires 'Future::AsyncAwait' => 0;
	requires 'Net::Async::Redis::XS' => 0;
	requires 'IO::Async::Loop::Epoll' => 0;

	requires 'XML::PugiXML' => 0;
	requires 'YAML::PP' => 0;
	requires 'JSON::MaybeXS' => 0;
	requires 'Cpanel::JSON::XS' => 0;

	requires 'List::BinarySearch' => 0;
	requires 'List::BinarySearch::XS' => 0;

	requires 'DBI' => 0;
	requires 'DBD::Pg' => 0;
	requires 'DBIx::Class' => 0;

	requires 'Import::Into' => 0;
	requires 'namespace::autoclean' => 0;
	requires 'all' => 0;
	requires 'Class::Inspector' => 0;

	requires 'Log::Handler' => 0;

	requires 'Sereal::Encoder' => 0;
	requires 'Sereal::Decoder' => 0;

	requires 'Carp::Always' => 0;
	requires 'Path::Tiny' => 0;
	requires 'CryptX' => 0;
	requires 'Data::ULID::XS' => '0.002';
	requires 'Quantum::Superpositions::Lazy' => 0;
	requires 'Data::Entropy' => 0; # for Q::S::L
	requires 'Crypt::Bcrypt' => 0;
	requires 'Algorithm::QuadTree::XS' => '0.07';
	requires 'Sub::Quote' => 0;
	requires 'Sub::Install' => 0;
	requires 'Game::TileMap' => '1.001';
	requires 'Time::HiRes' => 0;
	requires 'enum' => 0;

	requires 'Faker' => 0;
};

on 'develop' => sub {
	requires 'Text::Levenshtein::BV' => 0;
	requires 'Dumbbench' => 0;
};

on 'test' => sub {
	requires 'Test::DB';
	requires 'Test2::V0';
	requires 'Test::Deep';
	requires 'Test::Spy' => '0.005';
	requires 'Value::Diff' => 0;
	requires 'Capture::Tiny' => 0;
};

# vim: ft=perl

