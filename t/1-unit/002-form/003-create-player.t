use Form::CreatePlayer;
use Game::Helpers;

use testheader;

test_data
	'validation should succeed' => [
		[{name => 'test', class => lore_class('Witchhunter')->id}],
	];

test_data
	'validation should fail' => [
		[
			{},
			{'name' => ['field is required'], 'class' => ['field is required']}
		],
		[
			{name => 'aa', class => lore_class('Witchhunter')->id},
			{'name' => [_tph Err::NAME_TOO_SHORT, 3]}
		],
		[
			{name => 'aaaaaaaaaaaaaaaaaaaaa', class => lore_class('Witchhunter')->id},
			{'name' => [_tph Err::NAME_TOO_LONG, 20]}
		],
		[
			{name => 'aaa5', class => lore_class('Witchhunter')->id},
			{'name' => [Err::NAME_MUST_CONSIST_OF_LETTERS]}
		],
		[
			{name => 'aaa aaa', class => lore_class('Witchhunter')->id},
			{'name' => [Err::NAME_MUST_CONSIST_OF_LETTERS]}
		],
		[
			{name => 'aaaaa', class => 'not an id'},
			{'class' => [Err::INVALID_ELEMENT]}
		],
		[
			{name => 'aaaaa', class => lore_attribute('Physical')->id},
			{'class' => [Err::INVALID_ELEMENT]}
		],
	];

test validation_should_succeed => sub ($data) {
	my $form = Form::CreatePlayer->new;
	$form->set_input($data);
	ok $form->valid, "form valid $_";
};

test validation_should_fail => sub ($data, $errors) {
	my $form = Form::CreatePlayer->new;
	$form->set_input($data);
	ok !$form->valid, "form invalid $_";
	is $form->errors_hash, $errors, "errors hash $_";
};

done_testing;

