class MainController < ApplicationController
  def index
    response = {
        response: [
            ok: true
        ] 
    }
    respond_to do |format|
        format.plist { render text:  response.to_plist }
        format.json { render text: response.to_json }
    end
  end

  def test_post_url_encoded
    puts "Incoming parameters:\n#{params}"
    response = {response: params}
    respond_to do |format|
        format.plist { render text: response.to_plist }
    end
  end

  def test_post_multipart_encoded
    #puts "Incoming parameters:\n#{params}"
    puts "\n\n\n\n\n=============================\nParameter types:\n"
    for param in params do
        puts "#{param[0]} = #{param[1].class}\n"
    end
    puts "\n\n\n\n"
 
    path = File.join("public", "picture_ololo.png")
    File.open(path, "wb") { |f| 
        data = params[:picture]
        if data.is_a?(String) 
            f.write(params[:picture]) 
        else
            f.write(params[:picture].read)
        end
    }

    respond_to do |format|
        format.plist { render text: {response: "ok"}.to_plist }
    end
  end

end
