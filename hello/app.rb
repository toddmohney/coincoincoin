require 'sequel'
require 'sinatra'

DB = Sequel.connect('postgres://coincoincoin:coincoincoin@localhost:5432/coincoincoin')

class Contract < Sequel::Model
  def to_json
    values.
      merge("abi" => JSON.parse(values.fetch(:abi))).
      to_json
  end
end

get '/' do
  File.read('./public/index.html')
end

get '/contracts' do
  contracts = Contract.where(name: "Congress", network_id: "15")
  contracts.map(&:to_json)
end
