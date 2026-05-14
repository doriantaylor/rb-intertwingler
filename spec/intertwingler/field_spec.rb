require 'spec_helper'

require 'intertwingler/field'

describe Intertwingler::Field do
  # base class to_s should complain about not being implemented
  it 'should complain if you attempt to serialize it' do
    expect { Intertwingler::Field.new('test').to_s }.to raise_error(NotImplementedError)
  end

  it 'should properly handle an integer header' do
    val = Intertwingler::Field['CONTENT_LENGTH'].new '31337'

    expect(val).to be_a(Intertwingler::Field::NonNegativeInteger)
    expect(val.value).to eql 31337
    expect(val.to_s).to eql '31337'
  end

  it 'should parse an individual type with parameters' do
    val = Intertwingler::Field['Content-Type'].new 'Text/HTML;version=5;foo="lol wut"'
    # should downcase and rearrange and truncate the integer
    expect(val.to_s).to eq 'text/html;foo="lol wut";version=5'
  end

  it 'should parse a naïve list' do
    val = Intertwingler::Field['Content-Encoding'].new 'gzip, base64'

    expect(val).to be_a(Intertwingler::Field::List)
    expect(val.value).to eq %w[gzip base64]
  end

  it 'should correctly normalize a set' do
    val = Intertwingler::Field['Accept-Ranges'].new 'dongs, bytes'

    expect(val).to be_a(Intertwingler::Field::Set)
    # expect(val.value).to eq Set[:bytes, :dongs]
    expect(val.to_s).to eq 'bytes, dongs'
  end

  it 'should correctly handle pairs of values' do
    val = Intertwingler::Field['Cache-Control'].new 'max-age=500, must-revalidate'
    expect(val).to be_a(Intertwingler::Field::Pairs)

    # pairs should be sorted by key
  end

  it 'should handle ranked/weighted values' do
    # weighted values should be normalized and sorted descending by weight
    val = Intertwingler::Field['Content-Language'].new 'en-CA, en-US, fr;q=0.8'
  end

  it 'should handle an `Accept` header' do
    # media type parameters should be sorted lexically ex `q` which
    # should be added last; tokens should
  end

  it 'should handle a `Link` header' do
  end

  it 'should handle an `Authorization` header' do
  end
end
