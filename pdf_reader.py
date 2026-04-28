import PyPDF2

def extract_text_from_pdf(pdf_file: str) -> [str]:  #Oletetaan saavan merkkijonon ja palauttaa listan merkkijonoista
    pdf_text = []
    with open(pdf_file, "rb") as pdf:
        reader = PyPDF2.PdfReader(pdf)
        for page in reader.pages:
            content = page.extract_text()
            if content:
                pdf_text.append(content)
    return pdf_text

tiedosto = input("Anna pdf-tiedoston nimi muodossa nimi.pdf:")
if __name__ == "__main__":
    # Lue PDF:n sisältö
    extracted_text = extract_text_from_pdf(tiedosto)

    # Avainsanat
    words = {
        "responsib": 0,
        "esg": 0,
        "sustain": 0,
        "diversity": 0,
        "ethic": 0,
        "climate": 0,
        "ecologic": 0,
        "debt": 0,
        "ebit": 0,
        "financial": 0,
        "uncertainty": 0,
        "market": 0,
        "environment": 0,
    }

    # Käy läpi PDF:n tekstit
    for text in extracted_text:
        sanat = text.lower().split()
        for sana in sanat:
            for key in words:
                if sana.startswith(key):
                    words[key] += 1

    # Tulosta tulokset
    for key, value in words.items():
        print(f"{key}: {value}")

                    